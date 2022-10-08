(* divmod : int * int -> int * int
 * REQUIRES: n >= 0, d > 0
 * ENSURES: divmod (n, d) ==> (q, r) with q * d + r = n, r < d, q >= 0, r >= 0.
 *)
fun divmod (n : int, d : int) : int * int =
  if n < d
  then (0, n)
  else
    let
      val (q, r) = divmod(n - d, d)
    in
      (q + 1, r)
    end
