structure GateDefn =
struct

  type qubit_idx = int

  datatype t =
    PauliY of qubit_idx
  | PauliZ of qubit_idx
  | Hadamard of qubit_idx
  | SqrtX of qubit_idx
  | Sxdg of qubit_idx
  | S of qubit_idx
  | Sdg of qubit_idx
  | X of qubit_idx
  | T of qubit_idx
  | Tdg of qubit_idx
  | CX of {control: qubit_idx, target: qubit_idx}
  | CZ of {control: qubit_idx, target: qubit_idx}
  | CCX of {control1: qubit_idx, control2: qubit_idx, target: qubit_idx}
  | Phase of {target: qubit_idx, rot: real}
  | CPhase of
      { control: qubit_idx
      , target: qubit_idx
      , rot: real (* rotation in [0,2pi) *)
      }

  (** fsim(theta, phi) :=
    *   [ [ 1,   0,              0,              0          ],
    *     [ 0,   cos(theta),     -i sin(theta),  0          ],
    *     [ 0,   -i sin(theta),  cos(theta),     0          ],
    *     [ 0,   0,              0,              e^(-i phi) ] ]
    *)
  | FSim of {left: qubit_idx, right: qubit_idx, theta: real, phi: real}
  | RZ of {rot: real, target: qubit_idx}
  | RY of {rot: real, target: qubit_idx}
  | RX of {rot: real, target: qubit_idx}
  | Swap of {target1: qubit_idx, target2: qubit_idx}
  | CSwap of {control: qubit_idx, target1: qubit_idx, target2: qubit_idx}
  | U of {target: qubit_idx, theta: real, phi: real, lambda: real}
  | Other of {name: string, params: real Seq.t, args: qubit_idx Seq.t}

  type gate = t

  fun getGateArgs (g: gate) = case g of
      PauliY i => [i]
    | PauliZ i => [i]
    | Hadamard i => [i]
    | SqrtX i => [i]
    | Sxdg i => [i]
    | S i => [i]
    | Sdg i => [i]
    | X i => [i]
    | T i => [i]
    | Tdg i => [i]
    | CX {control = i, target = j} => [i, j]
    | CZ {control = i, target = j} => [i, j]
    | CCX {control1 = i, control2 = j, target = k} => [i, j, k]
    | Phase {target = i, ...} => [i]
    | CPhase {control = i, target = j, ...} => [i, j]
    | FSim {left = i, right = j, ...} => [i, j]
    | RZ {target = i, ...} => [i]
    | RY {target = i, ...} => [i]
    | RX {target = i, ...} => [i]
    | Swap {target1 = i, target2 = j} => [i, j]
    | CSwap {control = i, target1 = j, target2 = k} => [i, j, k]
    | U {target = i, ...} => [i]
    | Other {args = args, ...} => Seq.toList args

  fun toString (g: gate) (qi: qubit_idx -> string) = case g of
      PauliY i => "y " ^ qi i
    | PauliZ i => "z " ^ qi i
    | Hadamard i => "h " ^ qi i
    | T i => "t " ^ qi i
    | Tdg i => "tdg " ^ qi i
    | SqrtX i => "sx " ^ qi i
    | Sxdg i => "sxdg " ^ qi i
    | S i => "s " ^ qi i
    | Sdg i => "sdg " ^ qi i
    | X i => "x " ^ qi i
    | CX {control, target} => "cx " ^ qi control ^ ", " ^ qi target
    | CZ {control, target} => "cz " ^ qi control ^ ", " ^ qi target
    | CCX {control1, control2, target} =>
      "ccx " ^ qi control1 ^ ", " ^ qi control2 ^ ", " ^ qi target
    | Phase {target, rot} =>
      "phase(" ^ Real.toString rot ^ ") " ^ qi target
    | CPhase {control, target, rot} =>
      "cphase(" ^ Real.toString rot ^ ") " ^ qi control ^ ", " ^ qi target
    | FSim {left, right, theta, phi} =>
      "fsim(" ^ Real.toString theta ^ ", " ^ Real.toString phi ^ ") "
      ^ qi left ^ ", " ^ qi right
    | RZ {rot, target} =>
      "rz(" ^ Real.toString rot ^ ") " ^ qi target
    | RY {rot, target} =>
      "ry(" ^ Real.toString rot ^ ") " ^ qi target
    | RX {rot, target} =>
      "rx(" ^ Real.toString rot ^ ") " ^ qi target
    | CSwap {control, target1, target2} =>
      "cswap " ^ qi control ^ ", " ^ qi target1 ^ ", " ^ qi target2
    | Swap {target1, target2} =>
      "swap " ^ qi target1 ^ ", " ^ qi target2
    | U {target, theta, phi, lambda} =>
      "u(" ^ Real.toString theta ^ ", " ^ Real.toString phi ^ ", " ^ Real.toString lambda ^ ") " ^ qi target
    | Other {name, params, args} =>
      let val pstr =
              if Seq.length params = 0 then
                "()"
              else
                "(" ^ Seq.iterate (fn (acc, e) => acc ^ ", " ^ Real.toString e)
                                  (Real.toString (Seq.nth params 0)) (Seq.drop params 1) ^ ")"
        val front = name ^ pstr
        val args =
            if Seq.length args = 0 then
              ""
            else
              Seq.iterate (fn (acc, i) => acc ^ ", " ^ qi i)
                          (qi (Seq.nth args 0)) (Seq.drop args 1)
      in
        front ^ " " ^ args
      end
end
