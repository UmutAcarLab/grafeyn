structure SparseState :>
sig
  (* sparse state is abstractly a set of (weight, BasisIdx.t) pairs:
   *   { (a0, |000>), (a1, |101>), ... }
   *)
  type t
  type state = t
  type weight = Complex.t
  type qubit_idx = int

  val size: state -> int

  val toString: {numQubits: int} -> state -> string

  val empty: state
  val initial: state

  val toSeq: state -> (BasisIdx.t * weight) Seq.t
  val fromSeq: (BasisIdx.t * weight) Seq.t -> state

  (* combine duplicates, eliminate zeros *)
  val compact: state -> state
  val merge: state * state -> state

  val compactFromTable: (BasisIdx.t, Complex.t) HashTable.t -> state
end =
struct

  (* Signature mostly comes from ORD_MAP, from SML/NJ lib. I'm making some
   * small changes, so putting the full signature (with changes) here for
   * reference.
   *)
  structure BasisIdxMap:
  sig
    type 'a map
    type 'a t = 'a map
    val empty: 'a map
    val isEmpty: 'a map -> bool
    val singleton: (BasisIdx.t * 'a) -> 'a map
    val insert: 'a map * BasisIdx.t * 'a -> 'a map
    val insert': ((BasisIdx.t * 'a) * 'a map) -> 'a map
    val insertWith: ('a * 'a -> 'a) -> 'a map * BasisIdx.t * 'a -> 'a map
    val insertWithi: (BasisIdx.t * 'a * 'a -> 'a)
                     -> 'a map * BasisIdx.t * 'a
                     -> 'a map
    val find: 'a map * BasisIdx.t -> 'a option
    val lookup: 'a map * BasisIdx.t -> 'a
    val inDomain: ('a map * BasisIdx.t) -> bool
    val remove: 'a map * BasisIdx.t -> 'a map * 'a
    val first: 'a map -> 'a option
    val firsti: 'a map -> (BasisIdx.t * 'a) option
    val numItems: 'a map -> int
    val listItems: 'a map -> 'a list
    val listItemsi: 'a map -> (BasisIdx.t * 'a) list
    val listKeys: 'a map -> BasisIdx.t list
    val collate: ('a * 'a -> order) -> ('a map * 'a map) -> order
    val unionWith: ('a * 'a -> 'a) -> ('a map * 'a map) -> 'a map
    val unionWithi: (BasisIdx.t * 'a * 'a -> 'a) -> ('a map * 'a map) -> 'a map
    val intersectWith: ('a * 'b -> 'c) -> ('a map * 'b map) -> 'c map
    val intersectWithi: (BasisIdx.t * 'a * 'b -> 'c)
                        -> ('a map * 'b map)
                        -> 'c map
    val mergeWith: ('a option * 'b option -> 'c option)
                   -> ('a map * 'b map)
                   -> 'c map
    val mergeWithi: (BasisIdx.t * 'a option * 'b option -> 'c option)
                    -> ('a map * 'b map)
                    -> 'c map
    val app: ('a -> unit) -> 'a map -> unit
    val appi: ((BasisIdx.t * 'a) -> unit) -> 'a map -> unit
    val map: ('a -> 'b) -> 'a map -> 'b map
    val mapi: (BasisIdx.t * 'a -> 'b) -> 'a map -> 'b map
    val foldl: ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val foldli: (BasisIdx.t * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val foldr: ('a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val foldri: (BasisIdx.t * 'a * 'b -> 'b) -> 'b -> 'a map -> 'b
    val filter: ('a -> bool) -> 'a map -> 'a map
    val filteri: (BasisIdx.t * 'a -> bool) -> 'a map -> 'a map
    val mapPartial: ('a -> 'b option) -> 'a map -> 'b map
    val mapPartiali: (BasisIdx.t * 'a -> 'b option) -> 'a map -> 'b map
    val exists: ('a -> bool) -> 'a map -> bool
    val existsi: (BasisIdx.t * 'a -> bool) -> 'a map -> bool
    val all: ('a -> bool) -> 'a map -> bool
    val alli: (BasisIdx.t * 'a -> bool) -> 'a map -> bool
  end =
  struct
    structure BasisIdxKey = struct open BasisIdx type ord_key = t end
    structure M = RedBlackMapFn(BasisIdxKey)
    open M
    type 'a t = 'a map
  end

  (* ====================================================================== *)

  type weight = Complex.t

  type state = (BasisIdx.t * weight) Seq.t

  type t = state

  type qubit_idx = int

  (* ====================================================================== *)

  val empty = Seq.empty ()

  val initial = Seq.singleton (BasisIdx.zeros, Complex.defaultReal 1.0)

  fun toString {numQubits} state =
    let
      fun element (bidx, weight) =
        BasisIdx.toString {numQubits = numQubits} bidx ^ " "
        ^ Complex.toString weight ^ "\n"

      (* This is a little silly, but we're working around a MPL bug. It's not
       * safe to call Complex.toString in parallel because Complex.toString
       * calls Real.fmt, and Real.fmt is an SML Basis Library function that
       * is not safe for parallelism. Ugh. So, let's convert to strings
       * sequentially...
       *)
      val elements = Array.tabulate (Seq.length state, element o Seq.nth state)
      val elemSeq: string Seq.t = ArraySlice.full elements

      fun charsFromString str =
        DelayedSeq.tabulate (fn i => String.sub (str, i)) (String.size str)

      val allChars = DelayedSeq.toArraySeq (DelayedSeq.flatten
        (DelayedSeq.map charsFromString (DelayedSeq.fromArraySeq elemSeq)))
    in
      CharVector.tabulate (Seq.length allChars, Seq.nth allChars)
    end

  fun size state = Seq.length state

  (* ====================================================================== *)

  fun toMap state =
    Seq.iterate
      (fn (map, (bidx, weight)) =>
         BasisIdxMap.insertWith Complex.+ (map, bidx, weight))
      (BasisIdxMap.empty) state
  (*
  Seq.reduce (BasisIdxMap.unionWith Complex.+) BasisIdxMap.empty
    (Seq.map BasisIdxMap.singleton state)
  *)

  fun fromMap statemap =
    Seq.fromList (BasisIdxMap.listItemsi statemap)

  fun merge (state1, state2) =
    fromMap (BasisIdxMap.unionWith Complex.+ (toMap state1, toMap state2))

  fun compact state =
    Seq.filter (fn (_, w) => Complex.isNonZero w) (fromMap (toMap state))

  fun toSeq state = compact state
  fun fromSeq elems = elems

  (* ======================================================================= *)

  fun compactFromTable (table: (BasisIdx.t, Complex.t) HashTable.t) : state =
    let
      fun intoWidx x =
        case x of
          NONE => NONE
        | SOME (bidx, weight) => if Complex.isNonZero weight then x else NONE
    in
      fromSeq (Seq.mapOption intoWidx (HashTable.unsafeViewContents table))
    end

end
