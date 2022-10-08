fun fst (x : int, y : int) : int = x

fun snd (x : int, y : int) : int = y

fun diag (x : int) : int * int = (x,x)

(* incr : int -> int
 * REQUIRES: true
 * ENSURES: incr x ==> the next integer after x
 *)
fun incr (x : int) : int = x + 1

val 2 = incr 1
val 5 = incr 4
val ~3 = incr ~4

(* add3 :
 * REQUIRES:
 * ENSURES:
 *)
fun add3 _ = raise Fail "Unimplemented" (* Delete this line and replace it with your code! *)

(* Tests for add3 *)


(* flip :
 * REQUIRES:
 * ENSURES:
 *)
fun flip _ = raise Fail "Unimplemented" (* Delete this line and replace it with your code! *)

(* Tests for flip *)


(* diff :
 * REQUIRES:
 * ENSURES:
 *)
fun diff _ = raise Fail "Unimplemented" (* Delete this line and replace it with your code! *)

(* Tests for diff *)


(* isZero :
 * REQUIRES:
 * ENSURES:
 *)
fun isZero _ = raise Fail "Unimplemented" (* Delete this line and replace it with your code! *)

(* Tests for isZero *)


(* detectZeros :
 * REQUIRES:
 * ENSURES:
 *)
fun detectZeros _ = raise Fail "Unimplemented" (* Delete this line and replace it with your code! *)

(* Tests for detectZeros *)
