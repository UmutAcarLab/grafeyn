structure Circuit:
sig
  type circuit = {numQubits: int, gates: Gate.t Seq.t}
  type t = circuit

  val toString: circuit -> string

  val numGates: circuit -> int
  val numQubits: circuit -> int

  val simulate: circuit -> SparseState.t

  val simulateSequential: circuit -> SparseState.t
end =
struct

  type circuit = {numQubits: int, gates: Gate.t Seq.t}
  type t = circuit

  val splitThreshold = CommandLineArgs.parseInt "split-threshold" 100

  fun numGates ({gates, ...}: circuit) = Seq.length gates
  fun numQubits ({numQubits = nq, ...}: circuit) = nq


  fun toString {numQubits, gates} =
    let
      val header = "qreg q[" ^ Int.toString numQubits ^ "];\n"

      fun qi i =
        "q[" ^ Int.toString i ^ "]"

      fun gateToString gate =
        case gate of
          Gate.PauliY i => "y " ^ qi i
        | Gate.PauliZ i => "z " ^ qi i
        | Gate.Hadamard i => "h " ^ qi i
        | Gate.T i => "t " ^ qi i
        | Gate.X i => "x " ^ qi i
        | Gate.CX {control, target} => "cx " ^ qi control ^ ", " ^ qi target
        | Gate.CPhase {control, target, rot} =>
            "cphase(" ^ Real.toString rot ^ ") " ^ qi control ^ ", " ^ qi target
    in
      Seq.iterate op^ header (Seq.map (fn g => gateToString g ^ ";\n") gates)
    end


  fun simulate {numQubits, gates} =
    let
      fun gate i = Seq.nth gates i
      val depth = Seq.length gates

      fun loop i state =
        if i >= depth then
          state
        else if SparseState.size state >= splitThreshold then
          let
            val state = SparseState.toSeq state
            val half = Seq.length state div 2
            val (statel, stater) =
              ForkJoin.par
                ( fn _ => loop i (SparseState.fromSeq (Seq.take state half))
                , fn _ => loop i (SparseState.fromSeq (Seq.drop state half))
                )
          in
            (* SparseState.compact (SparseState.merge (statel, stater)) *)
            SparseState.fromSeq
              (Seq.append (SparseState.toSeq statel, SparseState.toSeq stater))
          end
        else
          loop (i + 1) (Gate.applyState (gate i) state)
    in
      SparseState.compact (loop 0 SparseState.initial)
    end


  fun simulateSequential {numQubits, gates} =
    let
      fun gate i = Seq.nth gates i
      val depth = Seq.length gates

      fun loop acc i widx =
        if i >= depth then
          widx :: acc
        else
          case Gate.apply (gate i) widx of
            Gate.OutputOne widx' => loop acc (i + 1) widx'
          | Gate.OutputTwo (widx1, widx2) =>
              loop (loop acc (i + 1) widx1) (i + 1) widx2

      val initial = (BasisIdx.zeros, Complex.real 1.0)
      val result = loop [] 0 initial
    in
      SparseState.compact (SparseState.fromSeq (Seq.fromList result))
    end


(*
  fun simulate {numQubits: int} circuit =
    let
      fun dump isFirst state =
        let val front = if isFirst then "" else "--------\n"
        in print (front ^ SparseState.toString {numQubits = numQubits} state)
        end

      fun doGate (state, gate) =
        let val result = SparseState.compact (Gate.apply gate state)
        in dump false result; result
        end
    in
      dump true SparseState.initial;
      Seq.iterate doGate SparseState.initial circuit
    end
*)

end
