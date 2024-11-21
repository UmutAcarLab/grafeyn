structure DataFlowGraphUtil :>
sig

  type gate_idx = int

  (* Traversal Automaton State *)
  type state = { visited: bool array, indegree: int array }

  type data_flow_graph = DataFlowGraph.t

  val visit: data_flow_graph -> gate_idx -> state -> unit
  val frontier: state -> gate_idx Seq.t
  val initState: data_flow_graph -> state

  (* Switches edge directions *)
  val transpose: data_flow_graph -> data_flow_graph

  val scheduleWithOracle: data_flow_graph -> (gate_idx -> bool) -> (gate_idx Seq.t -> gate_idx) -> bool -> int -> gate_idx Seq.t Seq.t

  val scheduleWithOracle': data_flow_graph -> (gate_idx -> bool) -> ('state * gate_idx Seq.t -> gate_idx) -> bool -> int -> ('state * gate_idx Seq.t -> 'state) -> 'state -> 'state

  val scheduleCost: gate_idx Seq.t Seq.t -> (gate_idx -> bool) -> real
  val chooseSchedule: gate_idx Seq.t Seq.t Seq.t -> (gate_idx -> bool) -> gate_idx Seq.t Seq.t

  val gateIsBranching: data_flow_graph -> (gate_idx -> bool)
end =
struct

  type gate_idx = int

  type data_flow_graph = DataFlowGraph.t

  fun transpose ({gates, preds, succs, numQubits}: data_flow_graph) =
      { gates = gates,
        preds = succs,
        succs = preds,
        numQubits = numQubits }

  (*fun transpose ({gates = gs, deps = ds, indegree = is, numQubits = qs}: data_flow_graph) =
      let val N = Seq.length gs
          val ds2 = Array.array (N, nil)
          fun apply i = Seq.map (fn j => Array.update (ds2, j, i :: Array.sub (ds2, j))) (Seq.nth ds i)
          val _ = Seq.tabulate apply N
      in
        {gates = gs,
         deps = Seq.tabulate (fn i => Seq.rev (Seq.fromList (Array.sub (ds2, i)))) N,
         indegree = Seq.map Seq.length ds,
         numQubits = qs}
      end*)

  type state = { visited: bool array, indegree: int array }

  fun visit {succs = succs, ...} i {visited = vis, indegree = deg} =
      (
        (* Set visited[i] = true *)
        Array.update (vis, i, true);
        (* Decrement indegree of each i dependency *)
        Seq.map (fn j => Array.update (deg, j, Array.sub (deg, j) - 1)) (Seq.nth succs i);
        ()
      )

  fun frontier {visited = vis, indegree = deg} =
      let val N = Array.length vis
          fun iter i acc =
              if i < 0 then
                acc
              else
                iter (i - 1) (if (not (Array.sub (vis, i))) andalso
                                 Array.sub (deg, i) = 0
                              then i :: acc else acc)
      in
        Seq.fromList (iter (N - 1) nil)
      end

  fun initState (graph: data_flow_graph) =
      let val N = Seq.length (#gates graph)
          val vis = Array.array (N, false)
          val deg = Array.tabulate (N, Seq.length o Seq.nth (#preds graph))
      in
        { visited = vis, indegree = deg }
      end

  fun scheduleWithOracle' (graph: data_flow_graph) (branching: gate_idx -> bool) (choose: 'state * gate_idx Seq.t -> gate_idx) (disableFusion: bool) (maxBranchingStride: int) (apply: 'state * gate_idx Seq.t -> 'state) state =
      let val dgst = initState graph
          fun findNonBranching (i: int) (xs: gate_idx Seq.t) =
              if i = Seq.length xs then
                NONE
              else if branching (Seq.nth xs i) then
                findNonBranching (i + 1) xs
              else
                SOME (Seq.nth xs i)
          fun returnSeq thisStep acc = Seq.map Seq.fromList (Seq.rev (Seq.fromList (if List.null thisStep then acc else List.rev thisStep :: acc)))
          fun loadNonBranching (acc: gate_idx list) =
              case findNonBranching 0 (frontier dgst) of
                  NONE => acc
                | SOME i => (visit graph i dgst; loadNonBranching (i :: acc))
          fun loadNext numBranchingSoFar thisStep state =
              let val ftr = frontier dgst in
                if Seq.length ftr = 0 then
                  state
                else
                  (let val next = choose (state, ftr) in
                     visit graph next dgst;
                     if numBranchingSoFar + 1 >= maxBranchingStride then
                       loadNext 0 nil (apply (state, Seq.rev (Seq.fromList (loadNonBranching (next :: thisStep)))))
                     else
                       loadNext (numBranchingSoFar + 1) (loadNonBranching (next :: thisStep)) state
                   end)
              end
          fun loadNextNoFusion state =
              let val ftr = frontier dgst in
                if Seq.length ftr = 0 then
                  state
                else
                  (let val next = choose (state, ftr) in
                     visit graph next dgst;
                     loadNextNoFusion (apply (state, Seq.singleton next))
                   end)
              end
      in
        if disableFusion then
          loadNextNoFusion state
        else
          loadNext 0 (loadNonBranching nil) state
      end

  fun scheduleWithOracle (graph: data_flow_graph) (branching: gate_idx -> bool) (choose: gate_idx Seq.t -> gate_idx) (disableFusion: bool) (maxBranchingStride: int) = Seq.rev (Seq.fromList (scheduleWithOracle' graph branching (fn (_, x) => choose x) disableFusion maxBranchingStride (fn (gs, g) => g :: gs) nil))

  (*fun scheduleCost2 (order: gate_idx Seq.t Seq.t) (branching: gate_idx -> bool) =
      let val gates = Seq.flatten order
          val N = Seq.length gates
          fun iter i cost branchedQubits =
              if i = N then
                cost
              else
                iter (i + 1) (1.0 + cost + (if branching (Seq.nth gates i) then cost else 0.0))
      in
        iter 0 0.0 (Vector.tabulate (N, fn _ => false))
      end*)

  
  fun scheduleCost (order: gate_idx Seq.t Seq.t) (branching: gate_idx -> bool) =
      let val gates = Seq.flatten order
          val N = Seq.length gates
          fun iter i cost =
              if i = N then
                cost
              else
                iter (i + 1) (1.0 + (if branching (Seq.nth gates i) then cost * 1.67 else cost))
      in
        iter 0 0.0
      end

  fun chooseSchedule (orders: gate_idx Seq.t Seq.t Seq.t) (branching: gate_idx -> bool) =
      let fun iter i best_i best_cost =
              if i = Seq.length orders then
                Seq.nth orders best_i
              else
                let val cost = scheduleCost (Seq.nth orders i) branching in
                  if cost < best_cost then
                    (print ("Reduced cost from " ^ Real.toString best_cost ^ " to " ^ Real.toString cost ^ "\n"); iter (i + 1) i cost)
                  else
                    (print ("Maintained cost " ^ Real.toString best_cost ^ " over " ^ Real.toString cost ^ "\n"); iter (i + 1) best_i best_cost)
                end
      in
        iter 1 0 (scheduleCost (Seq.nth orders 0) branching)
      end

  (* B and C don't affect gate_branching, so pick arbitrarily *)
  structure Gate_branching = Gate (structure B = BasisIdxUnlimited
                                   structure C = Complex64)


  fun gateIsBranching ({ gates = gates, ...} : data_flow_graph) =
      let val branchSeq = Seq.map (fn g => Gate_branching.expectBranching (Gate_branching.fromGateDefn g)) gates in
        fn i => Seq.nth branchSeq i
      end

end
