structure QuerySimSingleGoalWaveBFS:
sig
  (* first arg is space constraint, measured in terms of number of sparse
   * elements.
   * increasing this uses more space and decreases time.
   *)
  val query: int -> Circuit.t -> BasisIdx.t -> Complex.t
end =
struct

  fun query spaceConstraint {numQubits, gates} desired =
    let
      fun gate i = Seq.nth gates i
      val numGates = Seq.length gates

      val _ =
        if numQubits > 63 then raise Fail "whoops, too many qubits" else ()

      fun loopPushWave acc leftovers (gatenum, wave) =
        if gatenum >= numGates then
          (SimAccumulator.finishWave acc wave, leftovers)
        else
          let
            val sizeBefore = Wave.nonZeroSize wave
            val numWaves = 1 + WaveSet.numWaves leftovers
            val inUse = sizeBefore + WaveSet.totalSize leftovers
            (* val available = spaceConstraint - inUse
            val C = constraint available gatenum *)

            val acc = SimAccumulator.logSpaceUsage acc inUse
            val acc = SimAccumulator.logNumWaves acc numWaves

            val _ = print ("num waves  " ^ Int.toString numWaves ^ "\n")
            val _ = print ("using      " ^ Int.toString inUse ^ "\n")
            (* val _ = print ("available  " ^ Int.toString available ^ "\n")
            val _ = print ("constraint " ^ Int.toString C ^ "\n") *)

            val constraint = Int.max
              ( Int.max (2, sizeBefore)
              , spaceConstraint - WaveSet.totalSize leftovers
              )

            val {numGateApps, result, leftover} =
              Wave.advanceAndSplit
                {constraint = constraint, gate = gate gatenum} wave

            val leftovers = WaveSet.insert leftovers (gatenum, leftover)
            val acc = SimAccumulator.logGateApps acc numGateApps
            (* val (leftovers, result) =
              WaveSet.pullMerge leftovers (gatenum + 1, result) *)

            val sizeAfter = Wave.nonZeroSize result
          in
            if
              WaveSet.isWaveAt (gatenum + 1) leftovers
            then
              ( print
                  ("wave-schedule: arriving at goal "
                   ^ Int.toString (gatenum + 1) ^ "\n")
              ; (acc, WaveSet.insert leftovers (gatenum + 1, result))
              )

            else if
              gatenum + 1 >= numGates orelse sizeAfter <= sizeBefore
              orelse WaveSet.isWaveAfter (gatenum + 1) leftovers
            then
              if WaveSet.isWaveAfter (gatenum + 1) leftovers then
                ( print
                    ("wave-schedule: pushing wave to "
                     ^ Int.toString (gatenum + 1) ^ "\n")
                ; loopPushWave acc leftovers (gatenum + 1, result)
                )
              else
                ( print
                    ("wave-schedule: advancing goal to "
                     ^ Int.toString (gatenum + 1) ^ "\n")
                ; loopPushWave acc leftovers (gatenum + 1, result)
                )

            else
              ( print
                  ("wave-schedule: found goal at " ^ Int.toString (gatenum + 1)
                   ^ "\n")
              ; (acc, WaveSet.insert leftovers (gatenum + 1, result))
              )
          end


      fun loop acc waves =
        if WaveSet.numWaves waves = 0 then
          acc
        else
          let
            val (leftovers, gatenum, wave) =
              WaveSet.removeSecondNewestOrNewest waves

            val (acc, waves) = loopPushWave acc leftovers (gatenum, wave)
          in
            loop acc waves
          end

      val initAcc = SimAccumulator.init {desired = desired}
      val initWaves = WaveSet.singleton
        (0, Wave.singleton (BasisIdx.zeros, Complex.real 1.0))

      val {numGateApps, maxSpaceUsage, maxNumWaves, weight} =
        SimAccumulator.view (loop initAcc initWaves)

      val _ = print ("gate app count " ^ Int.toString numGateApps ^ "\n")
      val _ = print ("max space      " ^ Int.toString maxSpaceUsage ^ "\n")
      val _ = print ("max num waves  " ^ Int.toString maxNumWaves ^ "\n")
    in
      weight
    end

end
