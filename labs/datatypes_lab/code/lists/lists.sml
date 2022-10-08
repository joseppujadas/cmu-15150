(* merge : int list * int list -> int list
 * REQUIRES: L1 and L2 are sorted
 * ENSURES: merge (L1, L2) evaluates to a sorted list L that contains the
 * elements of L1 and L2
 *)
fun merge ([] : int list, l2 : int list) : int list = l2
  | merge (l1 : int list, [] : int list) : int list = l1
  | merge (x::xs : int list, y::ys : int list) : int list =
  if x < y then x::merge(xs,y::ys)
  else y::merge(x::xs,ys)

val [1,2,3,4,5] = merge([1,4],[2,3,5])
val [4,8,15,16,23,42] = merge ([16],[4,8,15,23,42])


(* head: int list -> int option
 * REQUIRES: true
 * ENSURES: head L ==>* SOME x if L is nonempty, where x is the first element
 *          of L, and NONE otherwise.
 *)
fun head ([] : int list) : int option = NONE
  | head (x::xs : int list) : int option = SOME x

(* credit: int list -> int list
 * REQUIRES: true
 * ENSURES: credit [x1, ..., xn] ==>* [x1 + 3, ..., xn + 3]
 *)
fun credit ([] : int list) : int list = []
  | credit (x::xs : int list) : int list = (3+x)::credit(xs)

val [4,8,15,16,23,42] = credit [1,5,12,13,20,39] 

(* evens: int list -> int list
 * REQUIRES: true
 * ENSURES: evens L ==>* L' where L' is L with all odd
 *          numbers removed, and the remaining elements
 *          in the original order
 *)
fun evens (L : int list) : int list = raise Fail "Unimplemented"

(* lastPositive: int list -> int option
 * REQUIRES: true
 * ENSURES: lastPositive L ==>* SOME x where x is the last positive number in
            the list, if it exists, or NONE if no such x exists.
 *)
fun lastPositive (L : int list) : int option = raise Fail "Unimplemented"

(* sequence: int option list -> int list option
 * REQUIRES: true
 * ENSURES: sequence L ==>* NONE if L contains at least one NONE or
 * SOME [x1, x2, ..., xn] if L is of the form
 * [SOME x1, SOME x2, ..., SOME xn]
 *)
fun sequence (L : int option list) : int list option =
  raise Fail "Unimplemented"

(* bitAnd: int list * int list -> int list
 * REQUIRES: A and B only contain 1s and 0s
 * ENSURES: bitAnd (A,B) evaluates to a list with a logical AND performed on the
 * corresponding elements of the two lists
 *)
fun bitAnd (_ : int list, _ : int list) : int list = raise Fail "Unimplemented"

(* interleave : int list * int list -> int list
 * REQUIRES: true
 * ENSURES: interleave (A,B) evaluates to a list built by alternating the
 * elements in A and B
 *)
fun interleave (_ : int list, _ : int list) : int list = raise Fail "Unimplemented"
