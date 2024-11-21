functor FullSimBFS
  (structure B: BASIS_IDX
   structure C: COMPLEX
   structure HS: HYBRID_STATE
   structure G: GATE
   sharing B = HS.B = G.B
   sharing C = HS.C = G.C

   val disableFusion: bool
   val maxBranchingStride: int
   val gateScheduler: string
   val blockSize: int
   val maxload: real
   val doMeasureZeros: bool
   val denseThreshold: real
   val pullThreshold: real):
sig
  val run: DataFlowGraph.t
           -> {result: (B.t * C.t) option DelayedSeq.t, counts: int Seq.t}
end =
struct

  structure Expander =
    ExpandState
      (structure B = B
       structure C = C
       structure HS = HS
       structure G = G
       val denseThreshold = denseThreshold
       val blockSize = blockSize
       val maxload = maxload
       val pullThreshold = pullThreshold)

  val bits = Seq.fromList [ (*"▏",*)"▎", "▍", "▌", "▊"]

  fun fillBar width x =
    let
      val middleSize = width - 2
      val {frac, whole} = Real.split (x * Real.fromInt middleSize)
      val filledCount = Real.round whole
      val lastBit = Seq.nth bits (Real.floor
        (Real.fromInt (Seq.length bits) * frac))
    in
      String.concat (List.tabulate (middleSize, fn i =>
        if i < filledCount then "█"
        else if i = filledCount then lastBit
        else " "))
    end


  fun leftPad width x =
    CharVector.tabulate (Int.max (0, width - String.size x), fn _ => #" ") ^ x

  fun rightPad width x =
    x ^ CharVector.tabulate (Int.max (0, width - String.size x), fn _ => #" ")


  fun dumpState numQubits s =
    let
      val ss =
        case s of
          HS.Sparse sst => HS.SST.unsafeViewContents sst
        | HS.Dense ds => HS.DS.unsafeViewContents ds
        | HS.DenseKnownNonZeroSize (ds, _) => HS.DS.unsafeViewContents ds
    in
      Util.for (0, DelayedSeq.length ss) (fn i =>
        case DelayedSeq.nth ss i of
          NONE => ()
        | SOME (bidx, weight) =>
            let in
              print
                (B.toString {numQubits = numQubits, pretty = true} bidx ^ " "
                 ^ C.toString weight ^ "\n")
            end)
    end

  structure DGFQ = DynSchedFinishQubitWrapper
                     (structure B = B
                      structure C = C
                      structure HS = HS
                      val maxBranchingStride = maxBranchingStride
                      val disableFusion = disableFusion)

  structure DGI = DynSchedInterference
                    (structure B = B
                     structure C = C
                     structure HS = HS
                     val maxBranchingStride = maxBranchingStride
                     val disableFusion = disableFusion)
  structure DGN = DynSchedNaive
                    (structure B = B
                     structure C = C
                     structure HS = HS
                     val maxBranchingStride = maxBranchingStride
                     val disableFusion = disableFusion)

  val gateSched =
      case gateScheduler of
          "naive" => DGN.choose
        | "gfq" => DGFQ.choose
        | "interference" => DGI.choose
        | _ => raise Fail ("Unknown scheduler '" ^ gateScheduler ^ "'\n")

  fun run dfg (*{numQubits, gates}*) =
    let
      val gates = Seq.map G.fromGateDefn (#gates dfg)
      val numQubits = #numQubits dfg
      fun gate i = Seq.nth gates i
      val depth = Seq.length gates
      val dgstate = DataFlowGraphUtil.initState dfg

      val pickNextGate =
          let val f = gateSched dfg in
            fn (s, g) => f (s, g)
          end

      (* val _ =
        if numQubits > 63 then raise Fail "whoops, too many qubits" else () *)
      val maxNumStates = IntInf.pow (2, numQubits)
      (* val maxNumStates = Word64.toInt
        (Word64.<< (0w1, Word64.fromInt numQubits)) *)
      val maxCountSize = Int.min
        (10, String.size (IntInf.toString maxNumStates))
      val maxGateNameSize = String.size (Int.toString depth)

      fun padCount x =
        leftPad maxCountSize (Int.toString x)
      fun padGate x =
        leftPad maxGateNameSize (Int.toString x)

      fun dumpDensity (i, nonZeroSize, zeroSize, capacity) =
        let
          val density = Rat.make (IntInf.fromInt nonZeroSize, maxNumStates)
          val approxDensity = Rat.approx density
          val densityStr = Real.fmt (StringCvt.FIX (SOME 8)) approxDensity
          val zStr =
            case zeroSize of
              NONE => ""
            | SOME x => " zero " ^ padCount x
          val slackStr =
            case (zeroSize, capacity) of
              (SOME zs, SOME cap) =>
                " slack "
                ^
                leftPad 3 (Int.toString (Real.ceil
                  (100.0
                   * (1.0 - Real.fromInt (nonZeroSize + zs) / Real.fromInt cap))))
                ^ "%"
            | _ => ""
        in
          print
            (fillBar 12 approxDensity ^ " " ^ "gate " ^ padGate i ^ " density "
             ^ densityStr ^ " nonzero " ^ padCount nonZeroSize ^ zStr ^ slackStr);
          TextIO.flushOut TextIO.stdOut;
          density
        end

      fun getNumZeros state =
          case state of
              HS.Sparse sst => HS.SST.zeroSize sst
            | HS.Dense ds => 0 (*raise Fail "Can't do dense stuff!"*)
              (*DS.unsafeViewContents ds, DS.nonZeroSize ds, TODO exception*)
            | HS.DenseKnownNonZeroSize (ds, nz) => 0 (*raise Fail "Can't do dense stuff!"*)
              (*DS.unsafeViewContents ds, nz, TODO exception*)

      val initialState = HS.Sparse
        (HS.SST.singleton {numQubits = numQubits} (B.zeros, C.defaultReal 1.0))

      fun runloop () =
          DataFlowGraphUtil.scheduleWithOracle'

            (* data flow graph *)
            dfg

            (* gate is branching *)
            (fn i => G.expectBranching (Seq.nth gates i))

            (* select gate *)
            (fn ((state, numGateApps, counts, gatesVisitedSoFar), gates) => pickNextGate (state, gates))

            (* disable fusion? *)
            disableFusion

            (* if fusion enabled, what's the max # of branching gates to fuse? *)
            maxBranchingStride

            (* apply gate fusion seq, updating state *)
            (fn ((state, numGateApps, counts, gatesVisitedSoFar), theseGates) =>
                let val numGatesVisitedHere = Seq.length theseGates
                    val ({result, method, numNonZeros, numGateApps = apps}, tm) =
                        Util.getTime (fn () =>
                                         Expander.expand
                                           { gates = Seq.map (Seq.nth gates) theseGates
                                           , numQubits = numQubits
                                           , maxNumStates = maxNumStates
                                           , state = state
                                           , prevNonZeroSize = (case counts of h :: t => h | nil => 1)
                                     })
                                     
                    val seconds = Time.toReal tm
                    val millions = Real.fromInt apps / 1e6
                    val throughput = millions / seconds
                    val throughputStr = Real.fmt (StringCvt.FIX (SOME 2)) throughput
                    val density =
                        dumpDensity (gatesVisitedSoFar, numNonZeros, SOME (getNumZeros state), NONE)
                    val _ = print
                              (" hop " ^ leftPad 3 (Int.toString numGatesVisitedHere) ^ " "
                               ^ rightPad 11 method ^ " "
                               ^ Real.fmt (StringCvt.FIX (SOME 4)) seconds ^ "s throughput "
                               ^ throughputStr ^ "\n")
                in
                  (result, numGateApps + apps, numNonZeros :: counts, gatesVisitedSoFar + numGatesVisitedHere)
                end
            )

            (* initial state *)
            (initialState, 0, [], 0)

      val (finalState, numGateApps, counts, gatesVisited) = runloop ()
      val nonZeros = case finalState of
                       HS.Sparse sst => HS.SST.unsafeViewContents sst
                     | HS.Dense ds => HS.DS.unsafeViewContents ds
                     | HS.DenseKnownNonZeroSize (ds, nz) => HS.DS.unsafeViewContents ds
      val _ = print ("gate app count " ^ Int.toString numGateApps ^ "\n")
    in
      {result = nonZeros, counts = Seq.fromList counts}
    end
end
