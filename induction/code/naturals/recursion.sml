(* add : int * int -> int
 * REQUIRES: n, m >= 0
 * ENSURES: add (n, m) ==>* n + m
 *)
fun add (0 : int, n : int) : int = n
  | add (m : int, n : int) : int = 1 + add (m - 1, n)

(* TEST CASES *)
val 0 = add (0,0)
val 5 = add (0,5)
val 5 = add (5,0)
val 165 = add (15,150)
val 228 = add (15,213)


(* mult :
 * REQUIRES:
 * ENSURES:
 *)
(* Note: You may want to pattern-match for different cases. *)
fun mult (0 : int, n : int) : int = 0
  | mult (m : int, n : int) : int = add (n, mult (m - 1, n))

(* TEST CASES *)
(* Write test cases here *)
val 4 = mult (2,2)
val 25 = mult (5,5)
val 0 = mult (0,242)
val 72 = mult (9,8)
val 9594 = mult (78,123)


fun isPrimeHelp (n : int, m : int) =
    if m < 2
    then true
    else if n mod m = 0
    then false
    else
      isPrimeHelp (n, m - 1)
(* isPrime : int -> bool
 * REQUIRES: n > 1
 * ENSURES: isPrime n ==>* true if n is prime and false otherwise
 *)
fun isPrime (n : int) : bool = isPrimeHelp (n, n - 1)

val true = isPrime 2
val true = isPrime 3
val false = isPrime 4
val true = isPrime 5
val true = isPrime 6947
val false = isPrime 6948
val false = isPrime 100


(* TEST CASES *)
(* Write test cases here *)
