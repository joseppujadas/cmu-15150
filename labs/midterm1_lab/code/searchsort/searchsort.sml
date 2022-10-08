datatype tree = Empty | Node of tree * int * tree

(* nextsmallest : tree * int * int -> int *)
(* REQUIRES : true *)
(* ENSURES : nextsmallest (T, lo, acc) => lo' such that lo' is the smallest
             element in T satisfying lo < lo' < acc, if such an element exists
             in T. Otherwise, nextsmallest returns acc. *)
fun nextsmallest (Empty : tree, lo : int, acc : int) : int = acc
  | nextsmallest (Node(Empty,x,Empty), lo, acc) = if x < acc andalso lo < x then x else acc
  | nextsmallest (Node(L,x,R), lo, acc) =
  if x < acc andalso lo < x then Int.min(nextsmallest(L,lo,x),nextsmallest(R,lo,x))
  else Int.min(nextsmallest(L,lo,acc),nextsmallest(R,lo,acc))

val x = nextsmallest(Node(Node(Node(Node(Empty,6,Empty),2,Node(Empty,7,Empty)),4,Node(Node(Empty,2,Empty),~5,Node(Empty,23,Empty))),1,Empty), ~400, 99)

(* searchsort : tree * int * int -> int list *)
(* REQUIRES : The elements in T are unique. *)
(* ENSURES : searchsort (T, lo, hi) => L such that L is a sorted list of all
             elements x in T for which lo < x < hi. *)
fun searchsort(Empty : tree, lo : int, hi : int) : int = []
  | searchsort(Node(Empty,x,Empty),lo,hi) = [x]
  | searchsort(Node(L,x,R)) = nextsmallest(Node(L,x,R)) @ searchsort L @ searchsort R
