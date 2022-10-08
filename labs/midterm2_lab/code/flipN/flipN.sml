(* curriedCons : 'a -> 'a list -> 'a list
 * REQUIRES: true
 * ENSURES: For all a : t1, L : t1 list,
 * curriedCons a L = a::L
 *)
fun curriedCons _ = raise Fail "Unimplemented"

(* prependX : bool -> bool list list -> bool list list
 * REQUIRES: true
 * ENSURES: prependX b L = prependXToAll b L
 * for all b : bool and L : bool list list
 *)
val prependX : bool -> (bool list list -> bool list list) =
  raise Fail "Unimplemented"

(* flipN : int -> bool list -> bool list list
 * REQUIRES: n >= 0
 * ENSURES: flipN n L = L', where L' is the list of ways
 * to flip exactly n trues to falses in L
 * (in no particular order).
 *)
fun flipN _ = raise Fail "Unimplemented"
