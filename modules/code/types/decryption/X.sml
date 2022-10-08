signature FOO =
  sig
    type t
    val into : int -> t
    val out : t -> int
  end

structure Bar :> FOO =
  struct
    type t = int
    fun into x = x
    fun out x = x
  end

val x = 5 + (Bar.into 10)
