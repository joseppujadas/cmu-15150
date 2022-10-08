(* Tree definition *)
datatype tree = Empty | Node of tree * int * tree

(* depth: tree -> int
 * REQUIRES: true
 * ENSURES: depth T ==>* the maximum depth of T
 *)
fun depth (Empty : tree) : int = 0
  | depth (Node(L, x, R) : tree) =
  let val (l,r) = (depth L, depth R) in
    if l > r then 1 + l
    else 1 + r
  end

val 0 = depth Empty
val 1 = depth (Node(Empty,1,Empty))
val 2 = depth (Node (Empty, 4, Node (Empty,5,Empty)))

(* Tests for depth *)


(* leaves: tree -> int list
 * REQUIRES: true
 * ENSURES: leaves T ==>* the values at the leaf nodes of T
 *)
fun leaves (Empty : tree) : int list = []
  | leaves (Node(L,x,R) : tree)

(* Tests for depth *)
