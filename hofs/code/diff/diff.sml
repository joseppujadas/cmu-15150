type poly = int -> real


(* val realEq = Real.==, but write out with fun, so one can re-use file after infixr. *)
val delta = 0.01
infix 4 realEq
fun x realEq y = Real.abs (x - y) < delta

(* UTILITIES FOR YOUR CONVENIENCE *)

(*  polynomialEqual : poly * poly * int -> bool
*   REQUIRES: n>=0, p1 and p2 return a value for input 0,1,...,n-1
*   ENSURES: polynomialEqual (p1,p2,n) = true iff the first n coefficients are
*            equal (i.e. if p1 0 = p2 0 and p1 1 = p2 1 and ... and p1 (n-1) = p2 (n-1))
*)
fun polynomialEqual (p1 : poly, p2 : poly, 0) = true
  | polynomialEqual (p1, p2, n) = (p1 (n-1) realEq p2 (n-1))
                                   andalso polynomialEqual(p1,p2,n-1)

(*  polyToString : poly * int -> string
*   REQUIRES: n>=0, p returns a value for input 0,1,...,n
*   ENSURES: polyToString (p,n) returns a string representation of p, up to the
*            nth coefficient
*)
fun polyToString (p : poly, 0) = Real.toString (p 0)
  | polyToString (p : poly, n : int) = polyToString(p, n-1) ^
      (if (p n realEq 0.0)
         then ""
         else " + " ^ (Real.toString (p n)) ^ "x^" ^ Int.toString n)


(* differentiate : poly -> poly
 * REQUIRES: p is a valid polynomial (i.e. p n = 0.0 for n<0 and p n = 0.0 for
 * all but finitely many n)
 * ENSURES: differentiate p = p', where p' is the derivative of p
 *)
fun differentiate (p : poly) : poly =
    fn x => (real (x+1)) * p (x + 1)

(* integrate : poly -> (real -> poly)
 * REQUIRES: p is a valid polynomial (i.e. p n = 0.0 for n<0 and p n = 0.0 for
 * all but finitely many n)
 * ENSURES: integrate p = P, where P(c) is the antiderivative of p with constant
 * of integration c
 * Alternatively, you could say differentiate ((integrate p) c) = p for all
 * c : real
 *)
fun integrate (p : poly) : real -> poly =
    fn C => (fn 0 => C | x => (p (x-1))/(real x))
