signature BASIC_INTSET =
sig
  type t (* abstract *)

  val emp : t

  val ins : t -> int -> t
  val mem : t -> int -> bool
end
