signature SEQFNS =
sig
  val exists: ('a -> bool) -> 'a Seq.t -> bool
  val transpose: 'a Seq.t Seq.t -> 'a Seq.t Seq.t
end
