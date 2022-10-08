datatype tree = Empty | Node of tree * int * tree

datatype direction = LEFT | RIGHT

(* find : tree * int -> direction list option
 * REQUIRES: T contains no duplicates
 * ENSURES: find (T, v) => NONE if v is not in T, else
 *  SOME L, where L is a list of directions that can be used to traverse
 *  from the root of T to v
 *)
fun find (Empty : tree, v : int) : direction list option = NONE
  | find (Node(L,x,R) : tree, v : int) =
    if x = v then SOME []
    else
    let
      val (lsearch,rsearch) = (find(L,v), find(R,v))
    in
      case (lsearch,rsearch) of
      (SOME l, _) => SOME (LEFT :: l)
    | (_, SOME r) => SOME (RIGHT :: r)
    | (_, _) => NONE

    end

val t = Node(Node(Empty,12,Node(Node(Empty,7,Empty),1,Node(Empty,~8,Empty))),3,Node(Node(Empty,4,Node(Empty,5,Empty)),2,Empty))

val SOME [LEFT] = find(t,12)
val SOME [RIGHT, LEFT, RIGHT] = find(t, 5)
val SOME [LEFT, RIGHT, RIGHT] = find(t,~8)

(* follow : tree * direction list -> tree option
 * REQUIRES: true
 * ENSURES: follow (T, L) => NONE if traversing T according to the directions
 *   in L do not lead to a valid subtree of T, else SOME T', where T' is the
 *   subtree of T that is obtained by traversing T according to L
 *)
fun follow (T: tree, [] : direction list) : tree option = SOME (T)
  | follow (Empty, L) = NONE
  | follow (Node(L,x,R), l::ls) =
      let
        val branch = if l = LEFT then L else R
      in
        follow (branch,ls)
      end

val t = Node(Node(Node(Empty,2,Empty),5,Node(Empty,0,Empty)),4,Node(Empty,3,Node(Empty,1,Empty)))
val SOME (Node(Node(Empty,2,Empty),5,Node(Empty,0,Empty))) = follow(t,[LEFT])
val SOME (Node(Empty,3,Node(Empty,1,Empty))) = follow(t,[RIGHT])



(* common : direction list * direction list -> direction list
 * REQUIRES: true
 * ENSURES: common (L1, L2) => L, where L contains the longest prefix
 *   that is common to both L1 and L2
 *)
fun common ([] : direction list, _ : direction list) : direction list = []
  | common (_, []) = []
  | common (l1::ls1, l2::ls2) =
    if l1 = l2 then l1 :: common(ls1,ls2)
    else []

val [LEFT, RIGHT] = common([LEFT, RIGHT, LEFT], [LEFT, RIGHT, RIGHT])
val [] = common ([RIGHT, LEFT, RIGHT, LEFT], [LEFT])

(* lca : tree * int * int -> tree option
 * REQUIRES: T contains no duplicates, a <> b
 * ENSURES: lca (T,a,b) => NONE if a or b is not in T, else
 *   SOME (Node (L,x,R)) such that Node(L,x,R) is a subtree of T and either:
 *             - a is in L and b is in R
 *             - b is in L and a is in R
 *             - a = x and b is in L or R
 *             - b = x and a is in L or R
 *)
fun lca (Empty : tree, a : int, b : int) : tree option = NONE
  | lca (T, a, b) =
      let
        val (findA, findB) = (find (T,a), find ((T,b)))
      in
        case (findA, findB) of
          (SOME A, SOME B) => follow(T, common(A,B))
        | (_, _) => NONE
      end

val t = Node(Node(Node(Empty,2,Empty),5,Node(Empty,0,Empty)),4,Node(Empty,3,Node(Empty,1,Empty)))
