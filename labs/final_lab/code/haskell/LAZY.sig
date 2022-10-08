signature LAZY =
sig
  type 'a t

  val delay : (unit -> 'a) -> 'a t
  val force : 'a t -> 'a
end
