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

signature BAZ =
  sig
    type t
    val create : int -> t
    val destroy : t -> int
  end

functor Qux (Waldo : FOO) :> BAZ =
  struct
    type t = Waldo.t
    val create = Waldo.into
    val destroy = Waldo.out
  end

structure S = Qux (Bar)
structure S' = Qux (Bar)

fun f x = S.destroy x

val y = f (S'.create 10)
