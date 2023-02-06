structure Complex:
sig
  type t

  val toString: t -> string

  val real: real -> t
  val imag: real -> t

  val zero: t
  val i: t

  val ~ : t -> t
  val + : t * t -> t
  val * : t * t -> t
end =
struct
  datatype t = C of {re: real, im: real}

  val rtos = Real.fmt (StringCvt.FIX (SOME 3))

  fun toString (C {re, im}) =
    rtos re ^ " + " ^ rtos im ^ "i"

  fun real r = C {re = r, im = 0.0}
  fun imag i = C {re = 0.0, im = i}

  val zero = C {re = 0.0, im = 0.0}
  val i = C {re = 0.0, im = 1.0}

  fun neg (C {re, im}) =
    C {re = ~re, im = ~im}

  fun add (C x, C y) =
    C {re = #re x + #re y, im = #im x + #im y}

  fun mul (C {re = a, im = b}, C {re = c, im = d}) =
    C {re = a * c + b * d, im = a * d + b * c}

  val ~ = neg
  val op+ = add
  val op* = mul
end
