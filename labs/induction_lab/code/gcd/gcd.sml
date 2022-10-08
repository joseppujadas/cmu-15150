(* GCD: int * int -> int *)
(* REQUIRES: m >= 0, n >= 0 *)
(* ENSURES: GCD (m,n) ==>* GCD of m and n *)
fun GCD (0 : int, n : int) : int = n
  | GCD (m : int, n : int) : int = GCD (m-n, n)

(* Tests for GCD *)
