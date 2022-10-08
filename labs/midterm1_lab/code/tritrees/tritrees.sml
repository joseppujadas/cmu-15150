datatype tritree = Nub | Branch of tritree * tritree * tritree * int
datatype tree = Empty | Node of tree * int * tree

(* accleaves : tritree * int list -> int list *)
(* REQUIRES : true *)
(* ENSURES : accleaves (T, L) ==> (leaves T) @ L *)
fun accleaves (Nub : tritree, LS : int list) : int list = LS
  | accleaves (Branch(L,C,R,v), LS) =
    let
      val (resL, resC, resR) = (accleaves (L,LS), accleaves (C,LS), accleaves (R,LS))
    in
     if (resL, resC, resR) = (LS,LS,LS) then v :: LS
     else resL @ resC @ resR
    end


(* Test Cases *)

val [] = accleaves (Nub, [])
val [1, 2, 3] = accleaves (Nub, [1, 2, 3])
val treeOne = Branch (Branch (Nub, Nub, Nub, 2),
                      Branch (Nub, Nub, Nub, 3),
                      Branch (Nub, Nub, Nub, 4), 1)
val [2, 3, 4] = accleaves (treeOne, [])
val [2, 3, 4, 5] = accleaves (treeOne, [5])
val treeTwo = Branch (Branch (Branch (Nub, Nub, Nub, 5), Nub, Nub, 2),
                      Branch (Nub, Branch (Nub, Nub, Nub, 6), Nub, 3),
                      Branch (Nub, Branch (Nub, Nub, Nub, 7), Branch (Nub, Nub, Nub, 8), 4), 1)
val [5, 6, 7, 8] = accleaves (treeTwo, [])
val [5, 6, 7, 8, 9] = accleaves (treeTwo, [9])
val treeThree = Branch (Branch (Nub, Nub, Nub, 2),
                        Branch (Branch (Nub, Nub, Nub, 5), Nub, Branch (Nub, Nub, Nub, 6), 3),
                        Branch (Nub, Nub, Branch (Nub, Nub, Nub, 7), 4), 1)
val [2, 5, 6, 7] = accleaves (treeThree, [])
val [2, 5, 6, 7, 8, 9] = accleaves (treeThree, [8, 9])


(* trim : tritree -> tree *)
(* REQUIRES : true *)
(* ENSURES : trim T ==> T', a binary tree that contains the left
*            and right children of T but removes the center children
*            and all of their descendents.*)
fun trim (T : tritree) : tree = raise Fail "Unimplemented"

(* Test Cases *)
(*
val Empty = trim Nub
val Node (Node (Empty, 2, Empty), 1, Node (Empty, 4, Empty)) = trim treeOne
val Node (Node (Node (Empty, 5, Empty), 2, Empty), 1,
          Node (Empty, 4, Node (Empty, 8, Empty))) = trim treeTwo
val Node (Node (Empty, 2, Empty), 1,
          Node (Empty, 4, Node (Empty, 7, Empty))) = trim treeThree
*)
