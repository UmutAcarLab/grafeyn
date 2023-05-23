functor FullSimBFS(SST: SPARSE_STATE_TABLE):
sig
  val run: Circuit.t -> (BasisIdx.t * Complex.t) option DelayedSeq.t
end =
struct

  structure Expander = ExpandState(SST)
  structure DS = DelayedSeq


  val maxBranchingStride = CommandLineArgs.parseInt "bfs-max-branching-stride" 1
  val _ = print
    ("bfs-max-branching-stride " ^ Int.toString maxBranchingStride ^ "\n")


  val doMeasureZeros = CommandLineArgs.parseFlag "measure-zeros"
  val dontCompact = CommandLineArgs.parseFlag "dont-compact"


  fun findNextGoal gates gatenum =
    let
      fun loop (i, branching) =
        if i >= Seq.length gates then
          (i, branching)
        else if Gate.expectBranching (Seq.nth gates i) then
          if branching >= maxBranchingStride then (i, branching)
          else loop (i + 1, branching + 1)
        else
          loop (i + 1, branching)
    in
      loop (gatenum, 0)
    end


  fun run {numQubits, gates} =
    let
      fun gate i = Seq.nth gates i
      val depth = Seq.length gates

      val _ =
        if numQubits > 63 then raise Fail "whoops, too many qubits" else ()
      val maxNumStates = Word64.toInt
        (Word64.<< (0w1, Word64.fromInt numQubits))

      fun dumpDensity (i, nonZeroSize, zeroSize, capacity) =
        let
          val densityStr = Real.fmt (StringCvt.FIX (SOME 8))
            (Real.fromInt nonZeroSize / Real.fromInt maxNumStates)
          val zerosStr =
            case zeroSize of
              NONE => "??"
            | SOME x => Int.toString x
          val slackPct =
            case (zeroSize, capacity) of
              (SOME zs, SOME cap) =>
                Int.toString (Real.ceil
                  (100.0
                   * (1.0 - Real.fromInt (nonZeroSize + zs) / Real.fromInt cap)))
                ^ "%"
            | _ => "??"
        in
          print
            ("gate " ^ Int.toString i ^ ": non-zeros: "
             ^ Int.toString nonZeroSize ^ "; zeros: " ^ zerosStr ^ "; slack: "
             ^ slackPct ^ "; density: " ^ densityStr ^ "\n")
        end

      fun makeNewState cap = SST.make {capacity = cap, numQubits = numQubits}

      fun loop next prevNonZeroSize state =
        let
          val capacityHere = SST.capacity state

          val numZeros =
            if doMeasureZeros then SOME (SST.zeroSize state) else NONE

          val (nonZeros, nonZeroSize) =
            (* if dontCompact then
              let
                val elems = SST.unsafeViewContents state
                val nonZeroSize = SST.nonZeroSize state
              in
                (elems, nonZeroSize)
              end
            else *)
            let
              val nonZeros = SST.compact state
              val nonZeroSize = DelayedSeq.length nonZeros
            in
              (DelayedSeq.map SOME nonZeros, nonZeroSize)
            end

          val _ = dumpDensity (next, nonZeroSize, numZeros, SOME capacityHere)
        in
          if next >= depth then
            nonZeros
          else
            let
              val (goal, numBranchingUntilGoal) = findNextGoal gates next

              val rate = Real.max
                (1.0, Real.fromInt nonZeroSize / Real.fromInt prevNonZeroSize)
              val guess = Real.ceil (1.25 * rate * Real.fromInt nonZeroSize)

              (* val multiplier = if numBranchingUntilGoal = 0 then 1.25 else 2.5
              val guess = Real.ceil (multiplier * Real.fromInt nonZeroSize) *)
              val guess = Int.min (guess, Real.ceil
                (1.25 * Real.fromInt maxNumStates))

              val theseGates = Seq.subseq gates (next, goal - next)
              val state = Expander.expand
                { gates = theseGates
                , numQubits = numQubits
                , state = nonZeros
                , expected = guess
                }
            in
              loop goal nonZeroSize state
            end
        end

      val initialState =
        SST.singleton {numQubits = numQubits} (BasisIdx.zeros, Complex.real 1.0)

      (* val (totalGateApps, finalState) = loopGuessCapacity 0 0 initialState 
      val _ = print ("gate app count " ^ Int.toString totalGateApps ^ "\n") *)

      val finalState = loop 0 1 initialState
    in
      finalState
    end
end
