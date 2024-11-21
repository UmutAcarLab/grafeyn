functor HybridState
  (structure B: BASIS_IDX
   structure C: COMPLEX
   structure SST: SPARSE_STATE_TABLE
   structure DS: DENSE_STATE
   sharing B = SST.B = DS.B
   sharing C = SST.C = DS.C): HYBRID_STATE =
struct
  structure B = B
  structure C = C
  structure SST = SST
  structure DS = DS

  datatype state =
    Sparse of SST.t
  | Dense of DS.t
  | DenseKnownNonZeroSize of DS.t * int

  type t = state
end
