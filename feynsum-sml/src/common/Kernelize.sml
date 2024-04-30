functor Kernelize
  (structure B: BASIS_IDX
   structure C: COMPLEX):
sig
    structure G: GATE
    structure B: BASIS_IDX
    structure C: COMPLEX
    sharing G.B = B
    sharing G.C = C

    val cost: G.t -> int (* TODO: Delete this, just wanted to check types *)
    val computeCosts: DataFlowGraph.t -> DataFlowGraphUtil.state -> (int * int) Seq.t -> G.t Seq.t -> (G.t Seq.t * (int * int) Seq.t) (* TODO: Delete this, just wanted to check types *)
    val performGateFusion: DataFlowGraph.t -> G.t Seq.t
end =
struct
    structure B = B
    structure C = C
    structure G = Gate(structure B = B
                       structure C = C)
    structure U = DataFlowGraphUtil

    fun cost (g: G.t) =
        #maxBranchingFactor g
        (* + Helpers.exp2(G.numUniqueQubitsTouched g) *)

    fun printHelper seq i =
        if i >= Seq.length seq
        then
            print("\n")
        else
            let
                val tuple as (index, cost) = Seq.nth seq i
                val _ = print(Int.toString(i) ^ "(cut=" ^ Int.toString(index) ^ ", cost=" ^ Int.toString(cost) ^ ") ")
            in
                printHelper seq (i+1)
            end

    fun printSeq (seq: (int*int) Seq.t) =
        printHelper seq 0

    fun minIndex (seq: int Seq.t) (start: int) =
        if start >= (Seq.length seq)-1
        then
            start
        else
            let
                val other = minIndex seq (start+1)
            in
                if Seq.nth seq other = Int.min((Seq.nth seq start), (Seq.nth seq other))
                then
                    other
                else
                    start
            end

    (* Assume costArray is pairs of (index, cost) *)
    fun computeCosts (circuit: DataFlowGraph.t) (state: U.state) (costArray: (int * int) Seq.t) (gates: G.t Seq.t) =
        let
            val num_gates = Seq.length(costArray)

            (* TODO: Reorganize this *)
            val numQubits = #numQubits circuit
            val globalGateOrdering = Seq.map (G.fromGateDefn {numQubits = numQubits}) (#gates circuit)

            val frontier = U.frontier state
            val _ = print("Frontier:\n ")
            val _ = print(Seq.toString (Int.toString) frontier)
            val _ = print("\n")
        in
            (* Exit once frontier is empty *)
            if Seq.length (frontier) > 0
            then
                let
                    (* Calculate ideal costs given static gate order, and return a pair for the best new addition *)
                    fun computeCostsFixed (gateOrder: G.t Seq.t) =
                        let
                            (* val _ = print("computeCosts, num_gates=" ^ Int.toString(num_gates) ^ "\n") *)
                            val costs = Seq.tabulate (fn j =>
                                                         let
                                                             val fused_gate = G.fuse(Seq.subseq gateOrder (j, num_gates-j))
                                                             val tuple as (prior_index, prior_cost) = Seq.nth costArray j
                                                         in
                                                             prior_cost + (cost fused_gate)
                                                         end
                                                     ) num_gates
                            val bestIndex = minIndex costs 0
                            (* val _ = print(" cost array: \n  ") *)
                            (* val _ = printSeq (Seq.tabulate (fn j => (j, Seq.nth costs j)) (Seq.length costs)) *)
                            (* val _ = print(" Min index: " ^ Int.toString(bestIndex) ^ "\n") *)
                        in
                            (bestIndex, (Seq.nth costs bestIndex))
                        end

                    val orderingCosts = Seq.tabulate (fn i =>
                                                         computeCostsFixed (Seq.append (gates, Seq.singleton(Seq.nth globalGateOrdering (Seq.nth frontier i))))
                                                     )
                                                     (Seq.length frontier)
                    val frontierChoiceIndex = minIndex (Seq.map (fn pair => #2 pair) orderingCosts) 0
                    val bestGateIndex = Seq.nth frontier frontierChoiceIndex

                    val _ = print("Iter " ^ Int.toString(num_gates) ^ ": Best gate=" ^ Int.toString(bestGateIndex) ^ "\n  costs:")
                    val _ = print(Seq.toString (Int.toString) (Seq.map (fn pair => #2 pair) orderingCosts))
                    val _ = print("\n\n")

                    (* Lock in choice of gate *)
                    val _ = U.visit circuit bestGateIndex state
                    val newGates = Seq.append (gates, Seq.singleton(Seq.nth globalGateOrdering (Seq.nth frontier frontierChoiceIndex)))

                    val newCostArray = Seq.append(costArray, Seq.singleton(computeCostsFixed newGates))
                in
                    computeCosts circuit state newCostArray newGates
                end
            else
                (gates, costArray)
          end

    (* Given the costs of each kernel, return the best possible kernelization *)
    fun fuseGates gates costArray =
        let
            fun fuseGatesHelper gateList costArray i =
                let
                    val priorIndex = #1 (Seq.nth costArray i)
                    val fused = if i > 0
                        then
                            Seq.append((fuseGatesHelper (Seq.subseq gateList (0, priorIndex)) costArray priorIndex),
                                       Seq.singleton (G.fuse (Seq.subseq gateList (priorIndex, i-priorIndex))))
                        else
                            gateList
                in
                    fused
                end
        in
            (* TODO: Sanity check kernels *)
            fuseGatesHelper gates costArray (Seq.length gates)
        end

    fun performGateFusion circuit =
        let
            (* Settle on ideal gate ordering, and find optimal costs for this ordering *)
            val (gates, costArray) = computeCosts circuit (U.initState circuit) (Seq.singleton((0,0))) (Seq.empty())

            (* NOTE: Everything below this point is just printing *)

            val _ = print("Cost array:\n ")
            val _ = printSeq costArray
            (* Print each kernel *)
            fun loop gates i =
                if i < (Seq.length gates)
                then
                    let
                        val _ = print("Kernel " ^ Int.toString(i) ^ ":\n")
                        val qstr = String.concatWith ", " (List.map (Int.toString) (G.getGateArgs (Seq.nth gates i)) )
                        val _ = print("Qubits touched: " ^ qstr ^ "\n")
                        val _ = print("Unique qubits touched: " ^ Int.toString(G.numUniqueQubitsTouched (Seq.nth gates i)) ^ "\n")
                        (* Print gates within each kernel *)
                        fun innerloop defns j =
                            if j < (Seq.length defns)
                            then
                                let
                                    val defn = Seq.nth defns j
                                    val stringified: string = GateDefn.toString defn (fn j => Int.toString(j))
                                    val _ = print(stringified ^ " | ")
                                in
                                    innerloop defns (j+1)
                                end
                            else
                                ()
                        val _ = print("(")
                        val _ = innerloop (#defn (Seq.nth gates i)) 0
                        val _ = print(" maxBranchFactor: " ^ Int.toString(#maxBranchingFactor (Seq.nth gates i)))
                        val _ = print(")\n\n")
                    in
                        loop gates (i + 1)
                    end
                else
                    ()
            val _ = loop (fuseGates gates costArray) 0
        in
            (* Obtain optimal kernelization by backtracking through cost array *)
            fuseGates gates costArray
        end

  (* Old formulation for fusing, which does not reorder gates *)
  (*     fun mediocreFusion gates = *)
  (*         let *)
  (*             (* Create empty cost array *) *)
  (*             val costArray = computeCosts gates (Seq.singleton((0,0))) 1 *)

  (*             val _ = print("Cost array:\n ") *)
  (*             val _ = printSeq costArray *)
  (*             fun loop gates i = *)
  (*                 if i < (Seq.length gates) *)
  (*                 then *)
  (*                     let *)
  (*                         (* val qstr = Seq.toString (Int.toString) (G.getGateArgs (Seq.nth gates i)) *) *)
  (*                         val qstr = String.concatWith ", " (List.map (Int.toString) (G.getGateArgs (Seq.nth gates i)) ) *)
  (*                         val _ = print("Qubits touched: " ^ qstr ^ "\n") *)
  (*                         val _ = print("Num qubits touched: " ^ Int.toString(G.numUniqueQubitsTouched (Seq.nth gates i)) ^ "\n") *)
  (*                         fun innerloop defns j = *)
  (*                             if j < (Seq.length defns) *)
  (*                             then *)
  (*                                 let *)
  (*                                     val defn = Seq.nth defns j *)
  (*                                     val stringified: string = GateDefn.toString defn (fn j => Int.toString(j)) *)
  (*                                     val _ = print(stringified ^ " | ") *)
  (*                                 in *)
  (*                                     innerloop defns (j+1) *)
  (*                                 end *)
  (*                             else *)
  (*                                 () *)
  (*                         val _ = print("(") *)
  (*                         val _ = innerloop (#defn (Seq.nth gates i)) 0 *)
  (*                         val _ = print(" maxBranchFactor: " ^ Int.toString(#maxBranchingFactor (Seq.nth gates i))) *)
  (*                         val _ = print(")\n") *)
  (*                     in *)
  (*                         loop gates (i + 1) *)
  (*                     end *)
  (*                 else *)
  (*                     () *)
  (*             val _ = loop (fuseGates gates costArray) 0 *)
  (*         in *)
  (*             fuseGates gates costArray *)
  (*         end *)
  end
