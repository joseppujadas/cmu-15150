(* e : real *)
val e : real = 2.71828182845905

(* isOne : int -> bool
 * REQUIRES: true
 * ENSURES: evaluates to true if the input is 1 and false otherwise *)
fun isOne (1 : int) = true
  | isOne _ = false

(* isPositive : int -> bool
 * REQUIRES: true
 * ENSURES: evaluates to true if the input is positive and false otherwise *)
fun isPositive (n : int) = n > 0

(* fact : int -> int
 * REQUIRES: n >= 0
 * ENSURES: fact n evaluates to n factorial *)
fun fact (0 : int) : int = 1
  | fact n = n * fact (n - 1)

val result = fact 5

(* exp : real -> real
 * REQUIRES: true
 * ENSURES: (exp x) ==> e^x *)
fun exp (x : real) : real = Math.exp x


(* pi : real *)
val pi : real = 3.14159265358979

(* area : real -> real
 * REQUIRES: d >= 0
 * ENSURES: evaluates to the area of a circle with radius d *)
fun area (d : real) : real = pi * d * d


(* vol : real -> real
 * REQUIRES: r >= 0
 * ENSURES: evaluates to the volume of a sphere with radius r *)
fun vol (r : real) : real = 4.0 / 3.0 * pi * r * r * r
