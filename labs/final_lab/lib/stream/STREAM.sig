signature STREAM =
sig
  type 'a stream   (* abstract *)
  datatype 'a front = Empty | Cons of 'a * 'a stream

  exception EmptyStream


  (* Lazy Stream Delay and Exposure *)

  val delay : (unit -> 'a front) -> 'a stream
  val expose : 'a stream -> 'a front


  (* Stream Construction *)

  val empty : 'a stream
  val cons : 'a * 'a stream -> 'a stream
  val fromList : 'a list -> 'a stream
  val tabulate : (int -> 'a) -> 'a stream


  (* Deconstructing a Stream *)

  val null : 'a stream -> bool
  val hd : 'a stream -> 'a
  val take : 'a stream * int -> 'a list
  val toList : 'a stream -> 'a list


  (* Simple Transformations *)

  val tl : 'a stream -> 'a stream
  val drop : 'a stream * int -> 'a stream
  val append : 'a stream * 'a stream -> 'a stream


  (* Combinators and Higher-Order Functions *)

  val map : ('a -> 'b) -> 'a stream -> 'b stream
  val filter : ('a -> bool) -> 'a stream -> 'a stream
  val zip : 'a stream * 'b stream -> ('a * 'b) stream

end
