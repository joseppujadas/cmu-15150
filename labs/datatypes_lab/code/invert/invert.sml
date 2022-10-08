datatype tree =
    Empty
  | Node of (tree * int * tree)

(* treeToList: tree -> int list
 * REQUIRES: true
 * ENSURES: treeToList t returns a list obtained from
 *          the inorder traversal of t
 *)
fun treeToList (t : tree) : int list =
    case t of
      Empty => []
    | Node (l,x,r) => treeToList l @ (x :: (treeToList r))

(* invert: tree -> tree
 * REQUIRES: true
 * ENSURES: invert T ==> T', where T' is the "mirror image" of T
 *)
fun invert (Empty : tree) : tree = raise Fail "Unimplemented"

(* Tests for invert *)

