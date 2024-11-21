structure HashSet :>
sig
  type 'a t
  type 'a table = 'a t

  exception Full

  val make: {hash: 'a -> int, eq: 'a * 'a -> bool, capacity: int, maxload: real} -> 'a table
  val size: 'a table -> int
  val capacity: 'a table -> int
  val resize: 'a table -> 'a table
  val increaseCapacityTo: int -> 'a table -> 'a table
  val insert: 'a table -> 'a -> unit
  val lookup: 'a table -> 'a -> bool
  val compact: 'a table -> 'a Seq.t

  (* Unsafe because underlying array is shared. If the table is mutated,
   * then the Seq would not appear to be immutable.
   *
   * Could also imagine a function `freezeViewContents` which marks the
   * table as immutable (preventing further inserts). That would be a safer
   * version of this function.
   *)
  val unsafeViewContents: 'a table -> 'a option Seq.t
end =
struct

  datatype 'a t =
    T of
      { data: 'a option array
      , hash: 'a -> int
      , eq: 'a * 'a -> bool
      , maxload: real
      }

  exception Full

  type 'a table = 'a t


  fun make {hash, eq, capacity, maxload} =
    if capacity = 0 then
      raise Fail "HashTable.make: capacity 0"
    else
      let val data = SeqBasis.tabulate 5000 (0, capacity) (fn _ => NONE)
      in T {data = data, hash = hash, eq = eq, maxload = maxload}
      end


  fun unsafeViewContents (T {data, ...}) = ArraySlice.full data


  fun bcas (arr, i) (old, new) =
    MLton.eq (old, Concurrency.casArray (arr, i) (old, new))


  fun size (T {data, ...}) =
    SeqBasis.reduce 10000 op+ 0 (0, Array.length data) (fn i =>
      if Option.isSome (Array.sub (data, i)) then 1 else 0)


  fun capacity (T {data, ...}) = Array.length data


  fun insert' (T {data, hash, eq, maxload}) x force =
    let
      val n = Array.length data
      val tolerance = 20 * Real.ceil (1.0 / (1.0 - maxload))

      fun loop i probes =
        if not force andalso probes >= tolerance then
          raise Full
        else if i >= n then
          loop 0 probes
        else
          let val current = Array.sub (data, i) in
            case current of
              SOME y =>
                if eq (x, y) then () else loop (i + 1) (probes + 1)
            | NONE =>
                if bcas (data, i) (NONE, SOME x) then
                  ()
                else
                  loop i probes
          end

      val start = (hash x) mod (Array.length data)
    in
      loop start 0
    end


  fun insert s x = insert' s x false


  fun lookup (T {data, hash, eq, ...}) x =
    let
      val n = Array.length data
      val start = (hash x) mod n

      fun loop i =
        case Array.sub (data, i) of
          NONE => false
        | SOME y => eq (x, y) orelse loopCheck (i + 1)

      and loopCheck i =
        if i >= n then loopCheck 0 else (i <> start andalso loop i)
    in
      n <> 0 andalso loop start
    end


  fun increaseCapacityTo newcap (input as T {data, hash, eq, maxload}) =
    if newcap < capacity input then
      raise Fail "HashTable.increaseCapacityTo: new cap is too small"
    else
      let
        val new = make
          {hash = hash, eq = eq, capacity = newcap, maxload = maxload}
      in
        ForkJoin.parfor 1000 (0, Array.length data) (fn i =>
          case Array.sub (data, i) of
            NONE => ()
          | SOME x => (insert' new x true; ()));

        new
      end


  fun resize x =
    increaseCapacityTo (2 * capacity x) x


  fun compact (T {data, ...}) =
    ArraySlice.full (SeqBasis.tabFilter 2000 (0, Array.length data) (fn i =>
      Array.sub (data, i)))

end
