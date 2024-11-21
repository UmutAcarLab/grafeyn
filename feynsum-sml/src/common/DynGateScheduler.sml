signature DYN_GATE_SCHEDULER =
sig
  structure B: BASIS_IDX
  structure C: COMPLEX
  structure HS: HYBRID_STATE
  sharing B = HS.B
  sharing C = HS.C

  type gate_idx = int

  type t = DataFlowGraph.t -> (HS.t * gate_idx Seq.t -> gate_idx)

  val choose: t
end

functor DynSchedFinishQubitWrapper
  (structure B: BASIS_IDX
   structure C: COMPLEX
  structure HS: HYBRID_STATE
  sharing B = HS.B
  sharing C = HS.C
   val maxBranchingStride: int
   val disableFusion: bool
  ): DYN_GATE_SCHEDULER =
struct
  structure B = B
  structure C = C
  structure HS = HS

  type gate_idx = int

  type t = DataFlowGraph.t -> (HS.t * gate_idx Seq.t -> gate_idx)

  structure FQS = FinishQubitScheduler
                     (val maxBranchingStride = maxBranchingStride
                      val disableFusion = disableFusion)

  structure G = Gate
                  (structure B = B
                   structure C = C)

  fun choose (depgraph: DataFlowGraph.t) =
      let val f = FQS.scheduler5 depgraph in
        fn (_, gates) => f gates
      end
  
end

functor DynSchedNaive
  (structure B: BASIS_IDX
   structure C: COMPLEX
   structure HS: HYBRID_STATE
   sharing B = HS.B
   sharing C = HS.C
   val maxBranchingStride: int
   val disableFusion: bool
  ): DYN_GATE_SCHEDULER =
struct
  structure B = B
  structure C = C
  structure HS = HS

  type gate_idx = int

  type t = DataFlowGraph.t -> (HS.t * gate_idx Seq.t -> gate_idx)

  fun choose (depgraph: DataFlowGraph.t) = fn (_, gates) => Seq.nth gates 0
  
end


functor DynSchedInterference
  (structure B: BASIS_IDX
   structure C: COMPLEX
   structure HS: HYBRID_STATE
   sharing B = HS.B
   sharing C = HS.C
   val maxBranchingStride: int
   val disableFusion: bool
  ): DYN_GATE_SCHEDULER =
struct
  structure B = B
  structure C = C
  structure HS = HS

  type gate_idx = int

  type t = DataFlowGraph.t -> (HS.t * gate_idx Seq.t -> gate_idx)

  structure FQS = FinishQubitScheduler
                     (val maxBranchingStride = maxBranchingStride
                      val disableFusion = disableFusion)

  structure G = Gate
                  (structure B = B
                   structure C = C)

  datatype Branched = Uninitialized | Zero | One | Superposition

  fun joinBranches' (br, b01) =
      case (br, b01) of
        (Uninitialized, false) => Zero
      | (Uninitialized, true)  => One
      | (Zero, false) => Zero
      | (Zero, true)  => Superposition
      | (One, false) => Superposition
      | (One, true)  => One
      | (Superposition, _)  => Superposition

  fun joinBranches (br, br') =
      case (br, br') of
        (Uninitialized, b) => b
      | (a, Uninitialized) => a
      | (Zero, Zero) => Zero
      | (One, One) => One
      | (_, _) => Superposition

  fun calculateBranchedQubits (numQubits, sst) =
      let val nonZeros = DelayedSeq.mapOption (fn x => x) (HS.SST.unsafeViewContents sst)
          val branchedQubits = Seq.tabulate (fn qi => DelayedSeq.reduce joinBranches Uninitialized (DelayedSeq.map (fn (b, c) => if B.get b qi then One else Zero) nonZeros)) numQubits
          fun isbranched b =
              case b of
                  Uninitialized => raise Fail "Uninitialized in isbranched! This shouldn't happen"
                | Zero => false
                | One => false
                | Superposition => true
      in
        Seq.map isbranched branchedQubits
      end

  fun choose (depgraph: DataFlowGraph.t) =
      let val gates = Seq.map G.fromGateDefn (#gates depgraph)
          val branchSeq = Seq.map G.expectBranching gates
          fun branching i = Seq.nth branchSeq i
          val numQubits = #numQubits depgraph
      in
        fn (hs, gidxs) => case hs of
            HS.Dense d => Seq.nth gidxs 0
          | HS.DenseKnownNonZeroSize d => Seq.nth gidxs 0
          | HS.Sparse sst =>
           (* if dense, doesn't really matter what we pick? *)
           let val branchedQubits = calculateBranchedQubits (numQubits, sst)
               fun getBranching i =
                   Seq.reduce (fn ((br, nbr), (br', nbr')) => (br + br', nbr + nbr')) (0, 0)
                              (Seq.map (fn qi => if Seq.nth branchedQubits qi then
                                                   (1, 0) else (0, 1))
                                       (#touches (Seq.nth gates i)))
               fun pick i (best_g, best_diff) =
                   if i >= Seq.length gidxs then
                     best_g
                   else
                     let val g = Seq.nth gidxs i
                         val (br, nbr) = getBranching g
                         val diff = if branching g then br - nbr else nbr - br
                     in
                       pick (i + 1) (if diff > best_diff then
                                       (g, diff) else (best_g, best_diff))
                     end
               val (br0, nbr0) = getBranching (Seq.nth gidxs 0)
           in
             pick 1 (Seq.nth gidxs 0, if branching 0 then br0 - nbr0 else nbr0 - br0)
           end
    end
  
end


(*functor DataFlowGraphDynScheduler
  (structure B: BASIS_IDX
   structure C: COMPLEX
   structure SST: SPARSE_STATE_TABLE
   structure DS: DENSE_STATE
   structure G: GATE
   sharing B = SST.B = DS.B = G.B
   sharing C = SST.C = DS.C = G.C
   val blockSize: int
   val maxload: real
   val denseThreshold: real
   val pullThreshold: real): DYN_GATE_SCHEDULER =
struct
  type gate_idx = int
  structure Expander =
    ExpandState
      (structure B = B
       structure C = C
       structure SST = SST
       structure DS = DS
       structure G = G
       val denseThreshold = denseThreshold
       val blockSize = blockSize
       val maxload = maxload
       val pullThreshold = pullThreshold)

  ( * From a frontier, select which gate to apply next * )
  ( *       args    visit gates, update frontier    break fusion      initial frontier  gate batches * )
  ( * type t = args -> (gate_idx -> gate_idx Seq.t) -> (unit -> unit) -> gate_idx Seq.t -> gate_idx Seq.t Seq.t* )
  type t = DataFlowGraph.t -> (Expander.state * gate_idx Seq.t -> gate_idx)
end
*)
