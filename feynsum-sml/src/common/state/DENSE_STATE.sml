signature DENSE_STATE =
sig
  structure B: BASIS_IDX
  structure C: COMPLEX

  type t
  type state = t

  val make: {numQubits: int} -> state

  val pull: {numQubits: int}
            -> (B.t -> {weight: C.t, count: int})
            -> {result: state, nonZeroSize: int, totalCount: int}

  val size: state -> int
  val nonZeroSize: state -> int
  val zeroSize: state -> int
  val capacity: state -> int

  val unsafeViewContents: state -> (B.t * C.t) option DelayedSeq.t

  val lookup: state -> B.t -> C.t option
  val lookupDirect: state -> B.t -> C.t
  val insertAddWeights: state -> B.t * C.t -> unit
end
