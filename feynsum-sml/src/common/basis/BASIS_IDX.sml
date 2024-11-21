signature BASIS_IDX =
sig
  type t

  val maxNumQubits: int option (* NONE: unlimited *)

  (* pretty=true:  |010111000...⟩
   * pretty=false:  010111000...
   *)
  val toString: {numQubits: int, pretty: bool} -> t -> string

  val zeros: t

  val setTo: bool -> t -> QubitIdx.t -> t
  val set: t -> QubitIdx.t -> t
  val unset: t -> QubitIdx.t -> t
  val flip: t -> QubitIdx.t -> t
  val swap: t -> QubitIdx.t * QubitIdx.t -> t
  val get: t -> QubitIdx.t -> bool

  (* not a very meaningful comparison; this is just for storing it in
   * ordered maps and whatever.
   *)
  val compare: t * t -> order
  val equal: t * t -> bool
  val hash: t -> int

  val fromDenseIndex: int -> t
  val toDenseIndex: t -> int
end
