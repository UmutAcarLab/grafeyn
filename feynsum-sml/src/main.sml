structure CLA = CommandLineArgs

(* 32-bit or 64-bit precision for floating point arithmetic *)
val precision = CLA.parseInt "precision" 32
val _ = print ("precision " ^ Int.toString precision ^ "\n")

(* some parameters for the expand subroutine *)
val blockSize = CLA.parseInt "expand-block-size" 10000
val maxload = CLA.parseReal "expand-max-load" 0.9
val denseThreshold = CLA.parseReal "dense-thresh" 0.25
val pullThreshold = CLA.parseReal "pull-thresh" 0.8
val _ = print ("expand-block-size " ^ Int.toString blockSize ^ "\n")
val _ = print ("expand-max-load " ^ Real.toString maxload ^ "\n")
val _ = print ("dense-thresh " ^ Real.toString denseThreshold ^ "\n")
val _ = print ("pull-thresh " ^ Real.toString pullThreshold ^ "\n")

(* should the simulator print out info about how many zeros were eliminated
 * on each frontier? (default: no)
 *)
val doMeasureZeros = CLA.parseFlag "measure-zeros"
val _ = print
  ("measure-zeros? " ^ (if doMeasureZeros then "yes" else "no") ^ "\n")

(* gate scheduler name *)
val schedulerName = CLA.parseString "scheduler" "gfq"
val _ = print ("scheduler " ^ schedulerName ^ "\n")

(* input circuit *)
val inputName = CLA.parseString "input" ""
val _ = print ("input " ^ inputName ^ "\n")

(* =========================================================================
 * parse input
 *)

val _ = print
  ("-------------------------------\n\
   \--- input-specific specs\n\
   \-------------------------------\n")

fun parseQasm () =
    let
      fun handleLexOrParseError exn =
        let
          val e =
            case exn of
              SMLQasmError.Error e => e
            | other => raise other
        in
          TerminalColorString.print
            (SMLQasmError.show
               {highlighter = SOME SMLQasmSyntaxHighlighter.fuzzyHighlight} e);
          OS.Process.exit OS.Process.failure
        end

      val ast = SMLQasmParser.parseFromFile inputName
                handle exn => handleLexOrParseError exn

      val simpleCirc = SMLQasmSimpleCircuit.fromAst ast
    in
      DataFlowGraph.fromQasm (Circuit.fromSMLQasmSimpleCircuit simpleCirc)
    end

val dfg =
  case inputName of
    "" => Util.die ("missing: -input FILE.qasm")
  | _ =>
    if String.isSuffix ".qasm" inputName then
      parseQasm ()
    else if String.isSuffix ".json" inputName then
      DataFlowGraph.fromJSONFile inputName
    else
      raise Fail "Unknown file suffix, use .qasm or .json"

val _ = print ("-------------------------------\n")

val _ = print ("gates  " ^ Int.toString (Seq.length (#gates dfg)) ^ "\n")
val _ = print ("qubits " ^ Int.toString (#numQubits dfg) ^ "\n")

val showCircuit = CLA.parseFlag "show-circuit"
val _ = print ("show-circuit? " ^ (if showCircuit then "yes" else "no") ^ "\n")
val _ =
  if not showCircuit then
    ()
  else
    print
      ("=========================================================\n"
       ^ DataFlowGraph.toString dfg
       ^ "=========================================================\n")

(* ========================================================================
 * gate scheduling
 *)


val disableFusion = CLA.parseFlag "scheduler-disable-fusion"
val maxBranchingStride = CLA.parseInt "scheduler-max-branching-stride" 2

structure DGNB =
  GreedyNonBranchingScheduler
    (val maxBranchingStride = maxBranchingStride
     val disableFusion = disableFusion)

structure DGFQ =
  FinishQubitScheduler
    (val maxBranchingStride = maxBranchingStride
     val disableFusion = disableFusion)

fun print_sched_info () =
  let
    val _ = print
      ("-------------------------------------\n\
       \--- scheduler-specific args\n\
       \-------------------------------------\n")
    val _ = print
      ("scheduler-max-branching-stride " ^ Int.toString maxBranchingStride
       ^ "\n")
    val _ = print
      ("scheduler-disable-fusion? " ^ (if disableFusion then "yes" else "no")
       ^ "\n")
    val _ = print ("-------------------------------------\n")
  in
    ()
  end

type gate_idx = int
type Schedule = gate_idx Seq.t

val maxBranchingStride' = if disableFusion then 1 else maxBranchingStride

(* ========================================================================
 * mains: 32-bit and 64-bit
 *)

structure M_64_32 =
  MkMain
    (structure B = BasisIdx64
     structure C = Complex32
     val maxBranchingStride = maxBranchingStride
     val disableFusion = disableFusion
     val blockSize = blockSize
     val maxload = maxload
     val gateScheduler = schedulerName
     val doMeasureZeros = doMeasureZeros
     val denseThreshold = denseThreshold
     val pullThreshold = pullThreshold)

structure M_64_64 =
  MkMain
    (structure B = BasisIdx64
     structure C = Complex64
     val maxBranchingStride = maxBranchingStride
     val disableFusion = disableFusion
     val blockSize = blockSize
     val maxload = maxload
     val gateScheduler = schedulerName
     val doMeasureZeros = doMeasureZeros
     val denseThreshold = denseThreshold
     val pullThreshold = pullThreshold)

structure M_U_32 =
  MkMain
    (structure B = BasisIdxUnlimited
     structure C = Complex32
     val maxBranchingStride = maxBranchingStride
     val disableFusion = disableFusion
     val blockSize = blockSize
     val maxload = maxload
     val gateScheduler = schedulerName
     val doMeasureZeros = doMeasureZeros
     val denseThreshold = denseThreshold
     val pullThreshold = pullThreshold)

structure M_U_64 =
  MkMain
    (structure B = BasisIdxUnlimited
     structure C = Complex64
     val maxBranchingStride = maxBranchingStride
     val disableFusion = disableFusion
     val blockSize = blockSize
     val maxload = maxload
     val gateScheduler = schedulerName
     val doMeasureZeros = doMeasureZeros
     val denseThreshold = denseThreshold
     val pullThreshold = pullThreshold)

val basisIdx64Okay = #numQubits dfg <= 62

val main =
  case precision of
    32 => if basisIdx64Okay then M_64_32.main else M_U_32.main
  | 64 => if basisIdx64Okay then M_64_64.main else M_U_64.main
  | _ =>
      Util.die
        ("unknown precision " ^ Int.toString precision
         ^ "; valid values are: 32, 64")

(* ======================================================================== *)

val _ = main (inputName, dfg)
