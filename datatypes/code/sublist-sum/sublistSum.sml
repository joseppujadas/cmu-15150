(* sublistSum : int list * int -> int list option
 * REQUIRES: true
 * ENSURES: sublistSum(L,n) ==>* SOME(L') where L' is a sublist of L which sums
 *          to n. sublistSum(L,n) ==>* NONE if there is no such sublist
 *)
fun sublistSum ([] : int list, n : int) = if n = 0 then SOME [] else NONE
  | sublistSum (x::xs : int list, n : int) : int list option =
  case sublistSum(xs,n-x) of SOME L => SOME (x::L) | NONE => sublistSum(xs,n)
