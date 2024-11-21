functor DenseState (structure B: BASIS_IDX structure C: COMPLEX): DENSE_STATE =
struct
  structure B = B
  structure C = C
  structure R =
  struct open C.R val fromLarge = fromLarge IEEEReal.TO_NEAREST end

  type r = C.r

  datatype t =
    T of {packedWeights: r array (* 2x capacity, for manual unboxing *)}

  type state = t

  fun make {numQubits} =
    let
      val capacity = Word64.toInt (Word64.<< (0w1, Word64.fromInt numQubits))
      val zero = R.fromLarge 0.0
      val packedWeights =
        SeqBasis.tabulate 5000 (0, 2 * capacity) (fn _ => zero)
    in
      T {packedWeights = packedWeights}
    end


  fun pull {numQubits} f =
    let
      val capacity = Word64.toInt (Word64.<< (0w1, Word64.fromInt numQubits))
      val packedWeights = ForkJoin.alloc (2 * capacity)
      val (totalCount, nonZeroSize) =
        SeqBasis.reduce 5000 (fn ((a, b), (c, d)) => (a + c, b + d)) (0, 0)
          (0, capacity)
          (fn i =>
             let
               val {weight, count} = f (B.fromDenseIndex i)
               val (re, im) = C.view weight
             in
               Array.update (packedWeights, 2 * i, re);
               Array.update (packedWeights, 2 * i + 1, im);
               (count, if C.isNonZero weight then 1 else 0)
             end)
    in
      { result = T {packedWeights = packedWeights}
      , totalCount = totalCount
      , nonZeroSize = nonZeroSize
      }
    end


  fun size (T {packedWeights} : state) = Array.length packedWeights div 2
  fun capacity (T {packedWeights} : state) = Array.length packedWeights div 2


  fun unsafeViewContents' (T {packedWeights}) =
    let
      fun makeWeight i =
        C.make (Array.sub (packedWeights, 2 * i), Array.sub
          (packedWeights, 2 * i + 1))

      fun elem i = (B.fromDenseIndex i, makeWeight i)
    in
      DelayedSeq.tabulate elem (Array.length packedWeights div 2)
    end


  fun unsafeViewContents x =
    DelayedSeq.map
      (fn widx => if C.isNonZero (#2 widx) then SOME widx else NONE)
      (unsafeViewContents' x)


  fun nonZeroSize x =
    let
      val elems = unsafeViewContents' x
    in
      SeqBasis.reduce 5000 op+ 0 (0, DelayedSeq.length elems) (fn i =>
        let val (_, weight) = DelayedSeq.nth elems i
        in if C.isNonZero weight then 1 else 0
        end)
    end


  fun zeroSize x =
    let
      val elems = unsafeViewContents' x
    in
      SeqBasis.reduce 5000 op+ 0 (0, DelayedSeq.length elems) (fn i =>
        let val (_, weight) = DelayedSeq.nth elems i
        in if C.isZero weight then 1 else 0
        end)
    end


  fun lookupDirect (T {packedWeights}) bidx =
    let
      val i = B.toDenseIndex bidx
      val weight = C.make (Array.sub (packedWeights, 2 * i), Array.sub
        (packedWeights, 2 * i + 1))
    in
      weight
    end


  fun lookup ds bidx =
    let val weight = lookupDirect ds bidx
    in if C.isNonZero weight then SOME weight else NONE
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


  fun insertAddWeights (T {packedWeights}) (bidx, weight) =
    let
      val i = B.toDenseIndex bidx
      val (re, im) = C.view weight
    in
      atomicAdd packedWeights (2 * i) re;
      atomicAdd packedWeights (2 * i + 1) im
    end

end
