signature SEQUENCE =
sig

  type 'a t
  type 'a seq = 'a t    (* abstract *)

  exception Range of string


  (* Constructing a Sequence *)

  val empty : unit -> 'a seq
  val singleton : 'a -> 'a seq
  val tabulate : (int -> 'a) -> int -> 'a seq
  val fromList : 'a list -> 'a seq


  (* Deconstructing a Sequence *)

  val nth : 'a seq -> int -> 'a
  val null : 'a seq -> bool
  val length : 'a seq -> int
  val toList : 'a seq -> 'a list
  val toString : ('a -> string) -> 'a seq -> string
  val equal : ('a * 'a -> bool) -> 'a seq * 'a seq -> bool


  (* Simple Transformations *)

  val rev : 'a seq -> 'a seq
  val append : 'a seq * 'a seq -> 'a seq
  val flatten : 'a seq seq -> 'a seq
  val cons : 'a -> 'a seq -> 'a seq


  (* Combinators and Higher-Order Functions *)

  val filter : ('a -> bool) -> 'a seq -> 'a seq
  val map : ('a -> 'b) -> 'a seq -> 'b seq
  val reduce : ('a * 'a -> 'a) -> 'a -> 'a seq -> 'a
  val reduce1 : ('a * 'a -> 'a) -> 'a seq -> 'a
  val mapreduce : ('a -> 'b) -> 'b -> ('b * 'b -> 'b) -> 'a seq -> 'b
  val zip : ('a seq * 'b seq) -> ('a * 'b) seq
  val zipWith : ('a * 'b -> 'c) -> 'a seq * 'b seq -> 'c seq


  (* Indexing-Related Functions *)

  val enum : 'a seq -> (int * 'a) seq
  val mapIdx : (int * 'a -> 'b) -> 'a seq -> 'b seq
  val update : ('a seq * (int * 'a)) -> 'a seq
  val inject : 'a seq * (int * 'a) seq -> 'a seq

  val subseq : 'a seq -> int * int -> 'a seq
  val take : 'a seq -> int -> 'a seq
  val drop : 'a seq -> int -> 'a seq
  val split : 'a seq -> int -> 'a seq * 'a seq


  (* Sorting and Searching *)

  val sort : ('a * 'a -> order) -> 'a seq -> 'a seq
  val merge : ('a * 'a -> order) -> 'a seq * 'a seq -> 'a seq
  val search : ('a * 'a -> order) -> 'a -> 'a seq -> int option


  (* Views *)

  datatype 'a lview = Nil | Cons of 'a * 'a seq

  val showl : 'a seq -> 'a lview
  val hidel : 'a lview -> 'a seq

  datatype 'a tview = Empty | Leaf of 'a | Node of 'a seq * 'a seq

  val showt : 'a seq -> 'a tview
  val hidet : 'a tview -> 'a seq

end
