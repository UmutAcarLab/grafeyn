(* mapping of qubit index to {0,1} *)
structure BasisIdx64 :> BASIS_IDX =
struct
  type qubit_idx = int

  type t = Word64.word
  val maxNumQubits = SOME 64

  fun fromDenseIndex i = Word64.fromInt i
  fun toDenseIndex t = Word64.toInt t

  val zeros: t = 0w0

  fun selector qi =
    Word64.<< (0w1, Word.fromInt qi)

  fun set bidx qi =
    Word64.orb (bidx, selector qi)

  fun unset bidx qi =
    Word64.andb (bidx, Word64.notb (selector qi))

  fun setTo b bidx qi =
    if b then set bidx qi else unset bidx qi

  fun flip bidx qi =
    Word64.xorb (bidx, selector qi)

  fun get bidx qi =
    0w0 <> (Word64.andb (bidx, selector qi))

  (* xor trick for swapping indices
   * https://graphics.stanford.edu/~seander/bithacks.html#SwappingBitsXOR
   *)
  fun swap bidx (qi1, qi2) =
    let
      val i = Word.fromInt qi1
      val j = Word.fromInt qi2
      val tmp = Word64.xorb (Word64.>> (bidx, i), Word64.>> (bidx, j))
      val tmp = Word64.andb (tmp, 0w1)
      val tmp = Word64.orb (Word64.<< (tmp, i), Word64.<< (tmp, j))
    in
      Word64.xorb (bidx, tmp)
    end


  fun toString {numQubits, pretty} bidx =
    (if pretty then "|" else "")
    ^
    CharVector.tabulate (numQubits, fn i =>
      if get bidx (numQubits - i - 1) then #"1" else #"0")
    ^ (if pretty then "⟩" else "")

  fun compare (bidx1: t, bidx2: t) = Word64.compare (bidx1, bidx2)

  fun equal (b1, b2) = b1 = b2

  fun hash bidx =
    Word64.toIntX (Util.hash64_2 bidx)
end
