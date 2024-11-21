structure BasisIdxUnlimited :> BASIS_IDX =
struct
  type qubit_idx = int
  type t = Word8.word Seq.t
  val maxNumQubits = NONE

  val zeros: t = Seq.empty ()

  fun indices qi = (qi div 8, qi mod 8)

  (* ==========================================================
   * operations on elems (Word8.word)
   *)

  structure Elem =
  struct
    fun selector ei =
      Word8.<< (0w1, Word.fromInt ei)

    fun set elem ei =
      Word8.orb (elem, selector ei)

    fun unset elem ei =
      Word8.andb (elem, Word8.notb (selector ei))

    fun setTo b elem ei =
      if b then set elem ei else unset elem ei

    fun flip elem ei =
      Word8.xorb (elem, selector ei)

    fun get elem ei =
      0w0 <> (Word8.andb (elem, selector ei))

    fun swap elem (ei1, ei2) =
      let
        val i = Word.fromInt ei1
        val j = Word.fromInt ei2
        val tmp = Word8.xorb (Word8.>> (elem, i), Word8.>> (elem, j))
        val tmp = Word8.andb (tmp, 0w1)
        val tmp = Word8.orb (Word8.<< (tmp, i), Word8.<< (tmp, j))
      in
        Word8.xorb (elem, tmp)
      end
  end

  (* ========================================================== *)

  fun show bidx =
    "BIDX("
    ^
    String.concatWith " " (List.tabulate (Seq.length bidx, fn i =>
      Word8.toString (Seq.nth bidx i))) ^ ")"

  fun compact bidx =
    let
      val maxNonzeroElemIndex =
        SeqBasis.reduce 5000 Int.max ~1 (0, Seq.length bidx) (fn i =>
          if Seq.nth bidx i <> 0w0 then i else ~1)
      val compactLen = maxNonzeroElemIndex + 1
    in
      if compactLen = Seq.length bidx then bidx
      else Seq.tabulate (Seq.nth bidx) compactLen
    end

  (* ========================================================== *)

  fun get bidx qi =
    let
      val (block_i, elem_i) = indices qi
    in
      if block_i >= Seq.length bidx then (* assume zero *) false
      else Elem.get (Seq.nth bidx block_i) elem_i
    end


  fun set bidx qi =
    let
      val (block_i, elem_i) = indices qi
      val newLen = Int.max (block_i + 1, Seq.length bidx)
      fun orig b =
        if b < Seq.length bidx then Seq.nth bidx b else 0w0
    in
      Seq.tabulate
        (fn b => if b = block_i then Elem.set (orig b) elem_i else orig b)
        newLen
    end


  fun unset bidx qi =
    let
      val (block_i, elem_i) = indices qi
      val newLen = Int.max (block_i + 1, Seq.length bidx)
      fun orig b =
        if b < Seq.length bidx then Seq.nth bidx b else 0w0
    in
      compact
        (Seq.tabulate
           (fn b => if b = block_i then Elem.unset (orig b) elem_i else orig b)
           newLen)
    end


  fun flip bidx qi =
    let
      val (block_i, elem_i) = indices qi
      val newLen = Int.max (block_i + 1, Seq.length bidx)
      fun orig b =
        if b < Seq.length bidx then Seq.nth bidx b else 0w0
    in
      compact
        (Seq.tabulate
           (fn b => if b = block_i then Elem.flip (orig b) elem_i else orig b)
           newLen)
    end


  fun setTo x bidx qi =
    let
      val (block_i, elem_i) = indices qi
      val newLen = Int.max (block_i + 1, Seq.length bidx)
      fun orig b =
        if b < Seq.length bidx then Seq.nth bidx b else 0w0
      val result =
        Seq.tabulate
          (fn b => if b = block_i then Elem.setTo x (orig b) elem_i else orig b)
          newLen
    in
      if x then result else compact result
    end


  fun swap bidx (qi1, qi2) =
    let
      val (block1_i, elem1_i) = indices qi1
      val (block2_i, elem2_i) = indices qi2
      val newLen = Int.max (Int.max (block1_i, block2_i) + 1, Seq.length bidx)
      fun orig b =
        if b < Seq.length bidx then Seq.nth bidx b else 0w0

      val result =
        if block1_i = block2_i then
          Seq.tabulate
            (fn b =>
               if b = block1_i then Elem.swap (orig b) (elem1_i, elem2_i)
               else orig b) newLen
        else
          Seq.tabulate
            (fn b =>
               if b = block1_i then
                 Elem.setTo (get bidx qi2) (orig b) elem1_i
               else if b = block2_i then
                 Elem.setTo (get bidx qi1) (orig b) elem2_i
               else
                 orig b) newLen
    in
      compact result
    end

  (* ========================================================== *)


  (* val get = fn bidx =>
    fn qi =>
      let
        val result = get bidx qi
      in
        print
          ("get " ^ show bidx ^ " " ^ Int.toString qi ^ " -> "
           ^ (if result then "1" else "0") ^ "\n");
        result
      end *)


  (* val set = fn bidx =>
    fn qi =>
      let
        val result = set bidx qi
      in
        print
          ("set " ^ show bidx ^ " " ^ Int.toString qi ^ " -> " ^ (show result)
           ^ "\n");
        result
      end
  
  val unset = fn bidx =>
    fn qi =>
      let
        val result = unset bidx qi
      in
        print
          ("unset " ^ show bidx ^ " " ^ Int.toString qi ^ " -> " ^ (show result)
           ^ "\n");
        result
      end *)


  (* ========================================================== *)


  (* TODO: could be more efficient *)
  fun compare (bidx1, bidx2) =
    let
      val hi = 8 * Int.max (Seq.length bidx1, Seq.length bidx2)
      val firstDiff = FindFirst.findFirst 1000 (0, hi) (fn i =>
        let
          val b1 = get bidx1 (hi - i - 1)
          val b2 = get bidx2 (hi - i - 1)
        in
          b1 <> b2
        end)
    in
      case firstDiff of
        NONE => EQUAL
      | SOME i => if get bidx1 (hi - i - 1) then GREATER else LESS
    end


  fun equal (bidx1, bidx2) =
    compare (bidx1, bidx2) = EQUAL


  fun hash bidx =
    let
      val maxNonzeroElemIndex =
        SeqBasis.reduce 5000 Int.max ~1 (0, Seq.length bidx) (fn i =>
          if Seq.nth bidx i <> 0w0 then i else ~1)
      val compactLen = maxNonzeroElemIndex + 1
    in
      if compactLen = 0 then
        Util.hash 0
      else
        Word64.toIntX (Util.loop (0, compactLen) 0w0 (fn (acc, i) =>
          let
            val topBits = Word64.andb (Word64.>> (acc, 0w56), 0wxFF)
            val newBits = Word64.fromInt (Word8.toInt (Seq.nth bidx i))
            val newBits' = Word64.xorb (newBits, topBits)
          in
            Util.hash64_2 (Word64.orb (Word64.<< (acc, 0w8), newBits'))
          end))
    end


  fun fromDenseIndex _ =
    raise Fail "BasisIdxUnlimited.fromDenseIndex: not yet implemented"

  fun toDenseIndex _ =
    raise Fail "BasisIdxUnlimited.toDenseIndex: not yet implemented"

  fun toString {numQubits, pretty} bidx =
    (if pretty then "|" else "")
    ^
    CharVector.tabulate (numQubits, fn i =>
      if get bidx (numQubits - i - 1) then #"1" else #"0")
    ^ (if pretty then "⟩" else "")
end
