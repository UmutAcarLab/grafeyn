structure GreedyBranchingScheduler:
sig
  val scheduler: GateScheduler.t
end =
struct

  type gate_idx = int

  fun pickBranching i branching gates =
      if i < Seq.length gates then
        (if branching i then
           Seq.nth gates i
         else
           pickBranching (i + 1) branching gates)
      else
        Seq.nth gates 0 (* pick a non-branching gate *)

  (* From a frontier, select which gate to apply next *)
  fun scheduler dg =
      let val branching = DataFlowGraphUtil.gateIsBranching dg in
        fn gates => pickBranching 0 branching gates
      end
end
