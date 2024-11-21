structure DataFlowGraph:
sig

  type gate_idx = int

  type data_flow_graph = {
    gates: GateDefn.t Seq.t,
    preds: gate_idx Seq.t Seq.t,
    succs: gate_idx Seq.t Seq.t,
    numQubits: int
  }

  type t = data_flow_graph

  val fromJSON: JSON.value -> data_flow_graph
  val fromJSONString: string -> data_flow_graph
  val fromJSONFile: string -> data_flow_graph
  val fromQasm: Circuit.t -> data_flow_graph

  val toString: data_flow_graph -> string
  (*val fromQasmString: string -> data_flow_graph*)
  (*val fromQasmFile: string -> data_flow_graph*)

  (*val mkGateDefn: (string * real list option * int list) -> GateDefn.t*)

end =
struct

  type gate_idx = int

  type data_flow_graph = {
    gates: GateDefn.t Seq.t,
    preds: gate_idx Seq.t Seq.t,
    succs: gate_idx Seq.t Seq.t,
    numQubits: int
  }

  type t = data_flow_graph

  fun expect opt err = case opt of
      NONE => raise Fail err
    | SOME x => x

  fun mkGateDefn ("x", NONE, [q0]) =
        GateDefn.X q0
    | mkGateDefn ("y", NONE, [q0]) =
        GateDefn.PauliY q0
    | mkGateDefn ("z", NONE, [q0]) =
        GateDefn.PauliZ q0
    | mkGateDefn ("h", NONE, [q0]) =
        GateDefn.Hadamard q0
    | mkGateDefn ("sx", NONE, [q0]) =
        GateDefn.SqrtX q0
    | mkGateDefn ("sxdg", NONE, [q0]) =
        GateDefn.Sxdg q0
    | mkGateDefn ("s", NONE, [q0]) =
        GateDefn.S q0
    | mkGateDefn ("sdg", NONE, [q0]) =
        GateDefn.S q0
    | mkGateDefn ("t", NONE, [q0]) =
        GateDefn.T q0
    | mkGateDefn ("tdg", NONE, [q0]) =
        GateDefn.Tdg q0
    | mkGateDefn ("cx", NONE, [control, target]) =
        GateDefn.CX {control = control, target = target}
    | mkGateDefn ("cz", NONE, [control, target]) =
        GateDefn.CZ {control = control, target = target}
    | mkGateDefn ("ccx", NONE, [control1, control2, target]) =
        GateDefn.CCX {control1 = control1, control2 = control2, target = target}
    | mkGateDefn ("phase", SOME [rot], [target]) =
        GateDefn.Phase {target = target, rot = rot}
    | mkGateDefn ("cp", SOME [rot], [control, target]) =
        GateDefn.CPhase {control = control, target = target, rot = rot}
    | mkGateDefn ("fsim", SOME [theta, phi], [left, right]) =
        GateDefn.FSim {left = left, right = right, theta = theta, phi = phi}
    | mkGateDefn ("rx", SOME [rot], [target]) =
        GateDefn.RX {rot = rot, target = target}
    | mkGateDefn ("ry", SOME [rot], [target]) =
        GateDefn.RY {rot = rot, target = target}
    | mkGateDefn ("rz", SOME [rot], [target]) =
        GateDefn.RZ {rot = rot, target = target}
    | mkGateDefn ("swap", NONE, [target1, target2]) =
        GateDefn.Swap {target1 = target1, target2 = target2}
    | mkGateDefn ("cswap", NONE, [control, target1, target2]) =
        GateDefn.CSwap {control = control, target1 = target1, target2 = target2}
    | mkGateDefn ("u", SOME [theta, phi, lambda], [target]) =
        GateDefn.U {target = target, theta = theta, phi = phi, lambda = lambda}
    | mkGateDefn ("u2", SOME [phi, lambda], [target]) =
        GateDefn.U {target = target, theta = Math.pi/2.0, phi = phi, lambda = lambda}
    | mkGateDefn ("u1", SOME [lambda], [target]) =
        GateDefn.U {target = target, theta = 0.0, phi = 0.0, lambda = lambda}
    | mkGateDefn (name, params, qargs) =
        raise Fail ("Unknown gate-params-qargs combination with name " ^ name)
  (*fun mkGate (g) = G.fromGateDefn (mkGateDefn g)*)

  fun arrayToSeq a = Seq.tabulate (fn i => Array.sub (a, i)) (Array.length a)

  fun getSuccsPredsFromJSON (edges, N) =
      let val preds = Array.array (N, nil);
          val succs = Array.array (N, nil);
          fun consAt (a, i, x) = Array.update (a, i, (x :: Array.sub (a, i)))
          fun go nil = ()
            | go (JSON.ARRAY [JSON.INT fm, JSON.INT to] :: edges) =
                let val fm64 = IntInf.toInt fm
                    val to64 = IntInf.toInt to
                    val _ = consAt (preds, to64, fm64)
                    val _ = consAt (succs, fm64, to64)
                in
                  go edges
                end
            | go (_ :: edges) = raise Fail "Malformed edge in JSON"
          val () = go edges;
          val toSeq = Seq.map (Seq.rev o Seq.fromList) o arrayToSeq
      in
        (toSeq preds, toSeq succs)
      end

  fun fromJSON (data) =
    let fun to_gate g =
          let val name = JSONUtil.asString (expect (JSONUtil.findField g "name") "Expected field 'name' in JSON");
              val params = Option.map (JSONUtil.arrayMap JSONUtil.asNumber) (JSONUtil.findField g "params");
              val qargs = JSONUtil.arrayMap JSONUtil.asInt (expect (JSONUtil.findField g "qargs") "Expected field 'qargs' in JSON");
          in
            mkGateDefn (name, params, qargs)
          end
        val numqs = case JSONUtil.findField data "qubits" of
                        SOME (JSON.INT qs) => IntInf.toInt qs
                      | _ => raise Fail "Expected integer field 'qubits' in JSON"
        val gates = case JSONUtil.findField data "nodes" of
                        SOME (JSON.ARRAY ns) => Seq.fromList (List.map to_gate ns)
                      | _ => raise Fail "Expected array field 'nodes' in JSON"
        val edges = case JSONUtil.findField data "edges" of
                        SOME (JSON.ARRAY es) => es
                      | _ => raise Fail "Expected array field 'nodes' in JSON"
        val (preds, succs) = getSuccsPredsFromJSON (edges, Seq.length gates)
    in
      { gates = gates,
        preds = preds,
        succs = succs,
        numQubits = numqs }
      (*Seq.zipWith (fn (g, (d, i)) => {gate = g, deps = d, indegree = i}) (gates, Seq.zip (deps, indegree))*)
    end
  fun fromJSONString (str) = fromJSON (JSONParser.parse (JSONParser.openString str))
  fun fromJSONFile (file) = fromJSON (JSONParser.parseFile file)

  (* TODO: convert to dependency graph *)
  fun fromQasm {numQubits, gates} =
      let val numGates = Seq.length gates
          val qubitLastGate = Array.array (numQubits, ~1)
          val preds = Array.array (numGates, nil)
          val succs = Array.array (numGates, nil)
          fun fillPreds gidx =
              if gidx >= numGates then
                ()
              else
                let val gate = Seq.nth gates gidx
                    val args = GateDefn.getGateArgs gate
                    val lasts = List.filter (fn i => i >= 0) (List.map (fn qidx => Array.sub (qubitLastGate, qidx)) args)
                    val _ = Array.update (preds, gidx, lasts)
                    val _ = List.map (fn gidx' => Array.update (succs, gidx', gidx :: Array.sub (succs, gidx'))) lasts
                    val _ = List.map (fn qidx => Array.update (qubitLastGate, qidx, gidx)) args
                in
                  fillPreds (gidx + 1)
                end
          val _ = fillPreds 0
          val predsSeq = Seq.map Seq.fromList (arrayToSeq preds)
          val succsSeq = Seq.map (Seq.rev o Seq.fromList) (arrayToSeq succs)
      in
        { gates     = gates,
          preds     = predsSeq,
          succs     = succsSeq,
          numQubits = numQubits }
      end

fun toString {gates, preds, succs, numQubits} =
    let val header = "qreg q[" ^ Int.toString numQubits ^ "];\n"
        fun qi i = "q[" ^ Int.toString i ^ "]"
    in
      Seq.iterate op^ header (Seq.map (fn g => GateDefn.toString g qi ^ ";\n") gates)
    end
end
