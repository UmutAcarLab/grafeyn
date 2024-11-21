structure Circuit:
sig
  type circuit = {numQubits: int, gates: GateDefn.t Seq.t}
  type t = circuit

  (*val toString: circuit -> string*)

  val numGates: circuit -> int
  val numQubits: circuit -> int

  val fromSMLQasmSimpleCircuit: SMLQasmSimpleCircuit.t -> circuit
end =
struct

  type circuit = {numQubits: int, gates: GateDefn.t Seq.t}
  type t = circuit

  fun numGates ({gates, ...}: circuit) = Seq.length gates
  fun numQubits ({numQubits = nq, ...}: circuit) = nq


  fun fromSMLQasmSimpleCircuit ({numQubits, gates}: SMLQasmSimpleCircuit.t) :
    circuit =
    let

      fun fail msg =
        raise Fail ("Circuit.fromSMLQasmSimpleCircuit: " ^ msg)


      fun evalLiteral x =
        case SMLQasmToken.getClass x of
          SMLQasmToken.DecimalIntegerLiteral =>
            let
              val str = SMLQasmToken.toString x
              val nice = String.implode
                (List.filter (fn c => c <> #"_") (String.explode str))
            in
              case Int.fromString nice of
                NONE => fail ("evalLiteral: error parsing '" ^ str ^ "'")
              | SOME x => Real.fromInt x
            end

        | SMLQasmToken.FloatLiteral =>
            let
              val str = SMLQasmToken.toString x
              val nice = String.implode
                (List.filter (fn c => c <> #"_") (String.explode str))
            in
              case Real.fromString nice of
                NONE => fail ("evalLiteral: error parsing '" ^ str ^ "'")
              | SOME x => x
            end

        | c =>
            fail ("unsupported literal class: " ^ SMLQasmToken.classToString c)


      fun evalIdentifier x =
        if SMLQasmToken.toString x = "pi" then Math.pi
        else fail ("unknown identifier: " ^ SMLQasmToken.toString x)


      fun evalExp e =
        case e of
          SMLQasmAst.Exp.Identifier x => evalIdentifier x

        | SMLQasmAst.Exp.Literal x => evalLiteral x

        | SMLQasmAst.Exp.Parens {exp, ...} => evalExp exp

        | SMLQasmAst.Exp.UnaryOp {opp, exp} =>
            let
              val r = evalExp exp
            in
              case SMLQasmToken.getClass opp of
                SMLQasmToken.Reserved SMLQasmToken.Minus => ~r
              | _ => fail ("unsupported unary op: " ^ SMLQasmToken.toString opp)
            end

        | SMLQasmAst.Exp.BinaryOp {exp1, opp, exp2} =>
            let
              val r1 = evalExp exp1
              val r2 = evalExp exp2
            in
              case SMLQasmToken.getClass opp of
                SMLQasmToken.Reserved SMLQasmToken.Plus => r1 + r2
              | SMLQasmToken.Reserved SMLQasmToken.Minus => r1 - r2
              | SMLQasmToken.Reserved SMLQasmToken.Asterisk => r1 * r2
              | SMLQasmToken.Reserved SMLQasmToken.Slash => r1 / r2
              | _ =>
                  fail ("unsupported binary op: " ^ SMLQasmToken.toString opp)
            end


      fun convertGate
        { gateName: string
        , params: SMLQasmAst.Exp.t Seq.t option
        , args: int Seq.t
        } =
        let
          val params =
            case params of
              NONE => Seq.empty ()
            | SOME ps => ps

          val paramArity = Seq.length params
          fun getParam i = Seq.nth params i
          val argArity = Seq.length args
          fun getArg i = Seq.nth args i
        in
          case (gateName, paramArity, argArity) of
            ("h", 0, 1) => GateDefn.Hadamard (getArg 0)

          | ("y", 0, 1) => GateDefn.PauliY (getArg 0)

          | ("z", 0, 1) => GateDefn.PauliZ (getArg 0)

          | ("s", 0, 1) => GateDefn.S (getArg 0)

          | ("sdg", 0, 1) => GateDefn.Sdg (getArg 0)

          | ("t", 0, 1) => GateDefn.T (getArg 0)

          | ("tdg", 0, 1) => GateDefn.Tdg (getArg 0)

          | ("x", 0, 1) => GateDefn.X (getArg 0)

          | ("sxdg", 0, 1) => GateDefn.Sxdg (getArg 0)

          | ("sx", 0, 1) => GateDefn.SqrtX (getArg 0)

          | ("cx", 0, 2) => GateDefn.CX {control = getArg 0, target = getArg 1}

          | ("cz", 0, 2) => GateDefn.CZ {control = getArg 0, target = getArg 1}

          | ("ccx", 0, 3) =>
              GateDefn.CCX
                {control1 = getArg 0, control2 = getArg 1, target = getArg 2}

          | ("phase", 0, 1) =>
              let val rot = evalExp (getParam 0)
              in GateDefn.Phase {rot = rot, target = getArg 0}
              end

          | ("cphase", 1, 2) =>
              let
                val rot = evalExp (getParam 0)
              in
                GateDefn.CPhase
                  {control = getArg 0, target = getArg 1, rot = rot}
              end

          | ("rx", 1, 1) =>
              GateDefn.RX {rot = evalExp (getParam 0), target = getArg 0}

          | ("ry", 1, 1) =>
              GateDefn.RY {rot = evalExp (getParam 0), target = getArg 0}

          | ("rz", 1, 1) =>
              GateDefn.RZ {rot = evalExp (getParam 0), target = getArg 0}

          | ("swap", 0, 2) =>
              GateDefn.Swap {target1 = getArg 0, target2 = getArg 1}

          | ("cswap", 0, 3) =>
              GateDefn.CSwap
                {control = getArg 0, target1 = getArg 1, target2 = getArg 2}

          | ("fsim", 2, 2) =>
              GateDefn.FSim
                { theta = evalExp (getParam 0)
                , phi = evalExp (getParam 1)
                , left = getArg 0
                , right = getArg 1
                }

          | ("u", 3, 1) =>
              GateDefn.U
                { target = getArg 0
                , theta = evalExp (getParam 0)
                , phi = evalExp (getParam 1)
                , lambda = evalExp (getParam 2)
                }

          | ("u3", 3, 1) =>
              GateDefn.U
                { target = getArg 0
                , theta = evalExp (getParam 0)
                , phi = evalExp (getParam 1)
                , lambda = evalExp (getParam 2)
                }

          | ("u2", 2, 1) =>
              GateDefn.U
                { target = getArg 0
                , theta = Math.pi / 2.0
                , phi = evalExp (getParam 0)
                , lambda = evalExp (getParam 1)
                }

          | ("u1", 1, 1) =>
              GateDefn.U
                { target = getArg 0
                , theta = 0.0
                , phi = 0.0
                , lambda = evalExp (getParam 0)
                }

          | _ =>
              GateDefn.Other
                {name = gateName, params = Seq.map evalExp params, args = args}
        end
    in
      {numQubits = numQubits, gates = Seq.map convertGate gates}
    end

end
