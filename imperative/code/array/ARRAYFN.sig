signature ARRAYFN =
sig
  type 'a t (* abstract *)
  val new: 'a -> int -> 'a t
  val len: 'a t -> int
  val nth: 'a t -> int -> 'a ref
  val doubleLen: 'a -> 'a t -> 'a t
  val toList: 'a t -> 'a list
end
