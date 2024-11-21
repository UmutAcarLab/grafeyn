functor SparseStateTableLockedSlots
  (structure B: BASIS_IDX structure C: COMPLEX): SPARSE_STATE_TABLE =
struct

  structure B = B
  structure C = C
  structure R =
  struct open C.R val fromLarge = fromLarge IEEEReal.TO_NEAREST end

  type r = C.r

  datatype t =
    T of
      { keys: B.t array
      , emptykey: B.t
      , lockedIdx: int
      , packedWeights: r array (* 2x capacity, for manual unboxing *)
      }

  exception Full
  exception DuplicateKey

  type table = t


  fun make' {capacity, emptykey, lockedIdx} =
    let
      val keys = SeqBasis.tabulate 5000 (0, capacity) (fn _ => emptykey)
      val zero = R.fromLarge 0.0
      val packedWeights =
        SeqBasis.tabulate 5000 (0, 2 * capacity) (fn _ => zero)
    in
      T { keys = keys
        , emptykey = emptykey
        , lockedIdx = lockedIdx
        , packedWeights = packedWeights
        }
    end


  fun checkSpaceForEmptyAndLockedKeys numQubits =
    numQubits >= 0
    andalso
    (case B.maxNumQubits of
       NONE => true
     | SOME limit => numQubits <= limit - 2)


  fun make {capacity, numQubits} =
    if capacity = 0 then
      raise Fail "SparseStateTableLockedSlots.make: capacity 0"
    else if not (checkSpaceForEmptyAndLockedKeys numQubits) then
      raise Fail "SparseStateTableLockedSlots.make: too many qubits"
    else
      let
        val emptykey = B.flip B.zeros (numQubits + 1)
        val lockedIdx = numQubits
      in
        make' {capacity = capacity, emptykey = emptykey, lockedIdx = lockedIdx}
      end


  fun capacity (T {keys, ...}) = Array.length keys


  fun size (T {keys, emptykey, ...}) =
    SeqBasis.reduce 10000 op+ 0 (0, Array.length keys) (fn i =>
      if B.equal (Array.sub (keys, i), emptykey) then 0 else 1)


  fun unsafeViewContents (T {keys, packedWeights, emptykey, ...}) =
    let
      fun makeWeight i =
        C.make (Array.sub (packedWeights, 2 * i), Array.sub
          (packedWeights, 2 * i + 1))

      fun elem i =
        let val k = Array.sub (keys, i)
        in if B.equal (k, emptykey) then NONE else SOME (k, makeWeight i)
        end
    in
      DelayedSeq.tabulate elem (Array.length keys)
    end


  fun bcas (arr, i, old, new) =
    MLton.eq (old, Concurrency.casArray (arr, i) (old, new))


  fun atomicAdd (arr: r array) i x =
    let
      val old = Array.sub (arr, i)
      val new = R.+ (old, x)
    in
      if bcas (arr, i, old, new) then () else atomicAdd arr i x
    end


  fun nonAtomicAdd (arr: r array) i x =
    Array.update (arr, i, R.+ (x, Array.sub (arr, i)))


  fun lockSlot i (table as T {keys, emptykey, lockedIdx, ...}) =
    let
      val current = Array.sub (keys, i)
    in
      if B.get current lockedIdx then lockSlot i table
      else if bcas (keys, i, current, B.set current lockedIdx) then current
      else lockSlot i table
    end


  (* requires newkey[lockedIdx] = 0 *)
  fun unlockSlot i (table as T {keys, emptykey, lockedIdx, ...}) newkey =
    Array.update (keys, i, newkey)


  fun atomicModifyAt i (table as T {keys, lockedIdx, packedWeights, ...}) f =
    let
      val bidx = lockSlot i table
      val weight = C.make (Array.sub (packedWeights, 2 * i), Array.sub
        (packedWeights, 2 * i + 1))
      val weight' = f weight
      val (re, im) = C.view weight'
    in
      Array.update (packedWeights, 2 * i, re);
      Array.update (packedWeights, 2 * i + 1, im);
      Array.update (keys, i, bidx)
    (* if bcas (keys, i, B.set bidx lockedIdx true, bidx) then ()
    else Util.die ("SparseStateTableLockedSlots !!!") *)
    end


  fun insertAddWeightsLimitProbes {probes = tolerance}
    (input as T {keys, packedWeights, lockedIdx, emptykey}) (x, v) =
    let
      val n = Array.length keys

      fun claimSlotAt i =
        bcas (keys, i, emptykey, B.set x lockedIdx)

      fun releaseSlotAt i = Array.update (keys, i, x)

      fun putValueAt i =
        let
          val (re, im) = C.view v
        in
          Array.update (packedWeights, 2 * i, re);
          Array.update (packedWeights, 2 * i + 1, im)
        end

      fun loop i probes =
        if probes >= tolerance then
          raise Full
        else if i >= n then
          loop 0 probes
        else
          let
            val current = Array.sub (keys, i)
            val k = B.unset current lockedIdx
          in
            if B.equal (k, emptykey) then
              if claimSlotAt i then (putValueAt i; releaseSlotAt i)
              else loop i probes
            else if B.equal (k, x) then
              atomicModifyAt i input (fn weight => C.+ (weight, v))
            else
              loop (i + 1) (probes + 1)
          end

      val start = (B.hash x) mod (Array.length keys)
    in
      loop start 0
    end


  fun insertAddWeights table (x, v) =
    insertAddWeightsLimitProbes {probes = capacity table} table (x, v)


  fun forceInsertUnique (T {keys, packedWeights, emptykey, lockedIdx}) (x, v) =
    let
      val n = Array.length keys
      val start = (B.hash x) mod n

      fun claimSlotAt i = bcas (keys, i, emptykey, x)

      fun putValueAt i =
        let
          val (re, im) = C.view v
        in
          nonAtomicAdd packedWeights (2 * i) re;
          nonAtomicAdd packedWeights (2 * i + 1) im
        end

      fun loop i =
        if i >= n then
          loop 0
        else
          let
            val current = Array.sub (keys, i)
            val k = B.unset current lockedIdx
          in
            if B.equal (k, emptykey) then
              if claimSlotAt i then putValueAt i else loop i
            else if B.equal (k, x) then
              raise DuplicateKey
            else
              loopNext (i + 1)
          end

      and loopNext i =
        if i = start then raise Full else loop i
    in
      loop start
    end


  fun lookup (T {keys, packedWeights, emptykey, ...}) x =
    let
      val n = Array.length keys
      val start = (B.hash x) mod n

      fun makeWeight i =
        C.make (Array.sub (packedWeights, 2 * i), Array.sub
          (packedWeights, 2 * i + 1))

      fun loop i =
        let
          val k = Array.sub (keys, i)
        in
          if B.equal (k, emptykey) then NONE
          else if B.equal (k, x) then SOME (makeWeight i)
          else loopCheck (i + 1)
        end

      and loopCheck i =
        if i >= n then loopCheck 0 else if i = start then NONE else loop i
    in
      if n = 0 then NONE else loop start
    end


  fun nonZeroSize state =
    let
      val currentElems = unsafeViewContents state
    in
      SeqBasis.reduce 1000 op+ 0 (0, DelayedSeq.length currentElems) (fn i =>
        case DelayedSeq.nth currentElems i of
          NONE => 0
        | SOME (bidx, weight) => if C.isNonZero weight then 1 else 0)
    end


  fun zeroSize state =
    let
      val currentElems = unsafeViewContents state
    in
      SeqBasis.reduce 1000 op+ 0 (0, DelayedSeq.length currentElems) (fn i =>
        case DelayedSeq.nth currentElems i of
          NONE => 0
        | SOME (bidx, weight) => if C.isNonZero weight then 0 else 1)
    end


  fun compact (T {keys, emptykey, packedWeights, ...}) =
    let
      fun makeWeight i =
        C.make (Array.sub (packedWeights, 2 * i), Array.sub
          (packedWeights, 2 * i + 1))

      fun makeElem i =
        ( Array.sub (keys, i)
        , Array.sub (packedWeights, 2 * i)
        , Array.sub (packedWeights, 2 * i + 1)
        )

      fun isNonZero x =
        not (C.realIsZero x)

      fun keepElem i =
        not (B.equal (Array.sub (keys, i), emptykey))
        andalso
        (isNonZero (Array.sub (packedWeights, 2 * i))
         orelse isNonZero (Array.sub (packedWeights, 2 * i + 1)))

      (* val data = SeqBasis.filter 5000 (0, Array.length keys) makeElem keepElem *)
      val keepers =
        SeqBasis.filter 5000 (0, Array.length keys) (fn i => i) keepElem

    (* val keepers = SeqBasis.tabFilter 5000 (0, Array.length keys) (fn i =>
      if keepElem i then SOME i else NONE) *)
    in
      (* DelayedSeq.tabulate
        (fn i => let val (b, re, im) = Array.sub (data, i)
                 in (b, C.make (re, im))
                 end) (Array.length data) *)

      DelayedSeq.tabulate
        (fn i => let val j = Array.sub (keepers, i)
                 in (Array.sub (keys, j), makeWeight j)
                 end) (Array.length keepers)
    end


  fun increaseCapacityByFactor alpha
    (table as T {keys, packedWeights, emptykey, lockedIdx}) =
    let
      val newCap = Real.ceil (alpha * Real.fromInt (capacity table))
      val newTable =
        make' {capacity = newCap, emptykey = emptykey, lockedIdx = lockedIdx}

    (* val elems = compact table *)
    in
      ForkJoin.parfor 1000 (0, capacity table) (fn i =>
        (* forceInsertUnique newTable (DelayedSeq.nth elems i)); *)
        let
          val key = Array.sub (keys, i)
          val weight = C.make (Array.sub (packedWeights, 2 * i), Array.sub
            (packedWeights, 2 * i + 1))
        in
          if C.isNonZero weight then forceInsertUnique newTable (key, weight)
          else ()
        end);

      newTable
    end


  fun singleton {numQubits} widx =
    let val table = make {capacity = 1, numQubits = numQubits}
    in forceInsertUnique table widx; table
    end

end
