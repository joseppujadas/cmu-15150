signature LAZYFNS =
sig
  structure Lazy : LAZY

  val && : bool Lazy.t * bool Lazy.t -> bool

  val pure: 'a -> 'a Lazy.t
  val map: ('a -> 'b) -> 'a Lazy.t -> 'b Lazy.t
  val join: 'a Lazy.t Lazy.t -> 'a Lazy.t

  val mkStream: 'a list Lazy.t -> 'a Stream.stream
end
