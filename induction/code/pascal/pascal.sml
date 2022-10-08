(* pascal : int * int -> int
 * REQUIRES: n >= k >= 0
 * pascal (n, k) ==>* the element of Pascal's triangle at position (n,k)
 *)
fun pascal (n : int, 0 : int) : int = 1
  | pascal (n : int, k : int) : int =
  if n = k
  then 1
  else
  pascal (n-1 , k-1) + pascal (n-1, k)

(* TEST CASES *)
(* Write test cases here *)
val 1 = pascal (0,0)
val 1 = pascal (43, 0)
val 4 = pascal (4,1)
val 6 = pascal (4,2)
val 20 = pascal (6,3)
val 2002 = pascal (14,9)
