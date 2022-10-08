(* evenP : int -> bool *)
(* REQUIRES: n >= 0 *)
(* ENSURES: evenP n evaluates to true if n mod 2 = 0 *)
(*          evenP n evaluates to false if n mod 2 = 1 *)
fun evenP (0 : int) : bool = true
  | evenP 1 = false
  | evenP n = evenP (n - 2)

(* Tests for evenP *)


(* oddP : int -> bool *)
(* REQUIRES: n >= 0 *)
(* ENSURES: oddP n ==>* true if is odd and false otherwise *)
fun oddP (0 : int) : bool = false
  | oddP (1 : int) : bool = true
  | oddP (n : int) = oddP (n - 2)

(* Tests for oddP *)
val false = oddP 0
val true = oddP 1
val true = oddP 2381927
val false = oddP 43200


(* Pattern of recursion for divisibleByThree:                 *)


(* divisibleByThree : int -> bool *)
(* REQUIRES: n >= 0 *)
(* ENSURES: divisibleByThree ==>* true if n is a multiple of 3 and false otherwise *)
fun divisibleByThree (0 : int) : bool = true
  | divisibleByThree (1 : int) : bool = false
  | divisibleByThree (2 : int) : bool = false
  | divisibleByThree (n : int) = divisibleByThree (n - 3)

(* Tests for divisibleByThree *)
val true = divisibleByThree 0
val false = divisibleByThree 1
val false = divisibleByThree 2
val true = divisibleByThree 3
val true = divisibleByThree 471
val false = divisibleByThree 262
