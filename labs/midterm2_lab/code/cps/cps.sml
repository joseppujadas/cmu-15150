datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree

(* prodCPS: 'a list * 'b list -> (('a * 'b) list -> 'c) -> 'c
 * REQUIRES: true
 * ENSURES: prodCPS (A, B) k = k L where L is  a list of length length A * length B
 *          consisting of all possible pairs (a, b) where a is an element of A
 *          and b is an element of B
 *)
fun prodCPS ([], _) k = k []
  | prodCPS (_, []) k = k []
  | prodCPS ([x], y::ys) k = prodCPS([x], ys) (fn res => k ((x,y)::res))
  | prodCPS (x::xs, y) k = prodCPS(xs,y) k

(* intersectCPS: ('a * 'a -> bool) -> 'a list * 'a list -> ('a list -> 'b) -> 'b
 * REQUIRES: eq is an equivalence relation
 * ENSURES: intersectCPS eq (L, R) k = k Si where S is a list containing all
 *          elements common to both L and R by eq.  Elements of S should appear
 *          in the same order they appeared in L.
 *)
fun intersectCPS eq (L, R) k = raise Fail "Unimplemented"

(* intersectC: ('a * 'a -> (bool -> 'b) -> 'b) ->
 *             'a list * 'a list ->
 *             ('a list -> 'b) ->
 *             'b
 * REQUIRES: eq is an equivalence relation
 * ENSURES: intersectC eq (L, R) k = k S where S is a list containing all
 *          elements x in L for which there exists an element y in R such that
 *          eq (x, y) k' = k' true.  Elements of S should appear in the same
 *          order they appeared in L.
 *)
fun intersectC eq (L, R) k = raise Fail "Unimplemented"

(* selectCPS: ('a -> bool) -> 'a tree -> ('a list -> 'b) -> 'b
 * REQUIRES: p is total
 * ENSURES: selectCPS p T k = k (select p T)
 *)
fun selectCPS p Empty k = k []
  | selectCPS p (Node(l,x,r)) k =
    selectCPS p l (fn res => selectCPS p r (fn res2 => if p x then k(res @ x::res2) else k(res @ res2)))

(* selectC: ('a -> (bool -> 'b) -> 'b) ->
 *          'a tree ->
 *          ('a list -> 'b) ->
 *          'b
 * REQUIRES: For all total pk, we have that p x pk is valuable
 * ENSURES: selectC p T k = selectCPS (fn x => p x (fn b => b)) T k
 *)
fun selectC p T k = raise Fail "Unimplemented"

fun length' [] k    = k 0
  | length' (x::xs) k = length' xs (fn res => k(1 + res))
