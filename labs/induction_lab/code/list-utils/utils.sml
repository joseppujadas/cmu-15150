(* addToEach :
 * REQUIRES:
 * ENSURES:
 *)
fun addToEach  _ = raise Fail "Unimplemented"

(* Tests for addToEach *)


(* mult :
 * REQUIRES:
 * ENSURES:
 *)
fun mult  _ = raise Fail "Unimplemented"

(* Tests for mult *)


(* trues :
 * REQUIRES:
 * ENSURES:
 *)
fun trues  _ = raise Fail "Unimplemented"

(* Tests for trues *)


(* trues' :
 * REQUIRES:
 * ENSURES:
 *)
fun trues'  _ = raise Fail "Unimplemented"

(* Tests for trues' *)


(* take : int * int list -> int list *  int list
 * REQUIRES: 0 <= i <= length L
 * ENSURES:
 *)
fun take (0 : int, L : int list) : int list * int list = ([], L)
  | take (i, L) =
    case L of
     []  => ([], [])
   | x::R => let val (A, B) = take (i - 1, R) in (x :: A, B)  end

(* bake : int * int list -> int list *  int list
 * REQUIRES: 0 <= i <= length L
 * ENSURES:
 *)
fun bake (i : int, L : int list): int list * int list =
if i = 0 then ([], L) else
  case L of
     []  => ([], [])
   | x::R => let val (A, B) = bake (i - 1, L) in (x :: A, B)
  end
