datatype ('a,'b) either = Left of 'a | Right of 'b
datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree

type bool' = (unit,unit) either
type 'a option' = (unit, 'a) either
datatype 'a list' = List of


(* inBool : bool' -> bool *)
(* REQUIRES : true *)
(* ENSURES : outBool (inBool b') = b' *)
fun inBool b' = raise Fail "Unimplemented"
(* outBool : bool -> bool' *)
(* REQUIRES : true *)
(* ENSURES : inBool (outBool b) = b *)
fun outBool b = raise Fail "Unimplemented"

(* Test Cases *)
(*
val true = inBool (outBool true)
val false = inBool (outBool false)
val Left () = outBool (inBool (Left ()))
val Right () = outBool (inBool (Right ()))
*)

(* inOpt : 'a option' -> 'a option *)
(* REQUIRES : true *)
(* ENSURES : outOpt (inOpt x') = x' *)
fun inOpt x' = raise Fail "Unimplemented"
(* outOpt : 'a option -> 'a option' *)
(* REQUIRES : true *)
(* ENSURES : inOpt (outOpt x) = x *)
fun outOpt x = raise Fail "Unimplemented"

(* Test Cases *)
(*
val SOME 1 = inOpt (outOpt (SOME 1))
val NONE = inOpt (outOpt NONE)
val Left () = outOpt (inOpt (Left ()))
val Right 1 = outOpt (inOpt (Right 1))
*)

(* inList : bool' -> bool *)
(* REQUIRES : true *)
(* ENSURES : outList (inList b') = b' *)
fun inList L' = raise Fail "Unimplemented"
(* outList : bool -> bool' *)
(* REQUIRES : true *)
(* ENSURES : inList (outList b) = b *)
fun outList L = raise Fail "Unimplemented"

(* Test Cases *)
(*
val [1,2,3] = inList (outList [1,2,3])
val [] = inList (outList [])
val List (Left ()) = outList (inList (List (Left ())))
val List (Right (1, List (Left ()))) = outList (inList (List (Right (1, List(
Left ())))))
*)

(* elim : ('a -> 'c) -> ('b -> 'c) -> ('a,'b) either -> 'c *)
(* REQUIRES : true *)
(* ENSURES : true*)
fun elim _ _ _ = raise Fail "Unimplemented"
