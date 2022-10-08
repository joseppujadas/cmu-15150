signature UTILS =
sig
  val collect : ('a * 'a -> order) -> 'a Seq.t -> 'a Seq.t Seq.t
  val unique  : ('a * 'a -> order) -> 'a Seq.t -> 'a Seq.t
end
