(* summ :
 * REQUIRES:
 * ENSURES: *)
fun summ (0 : int) : int = 0
  | summ (n : int) : int = n + summ (n - 1)

(* Tests *)
val 0 : int = summ 0
val 1 : int = summ 1
val 10 : int = summ 4
val 903 : int = summ 42
