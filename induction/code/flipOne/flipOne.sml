(* flipOneHelp : bool list * bool list * bool list list -> bool list list
 * REQUIRES : true
 * ENSURES : flipOneHelp prev, curr, acc => An ordered list containing all the ways to flip exactly
 * one true to false.
*)



fun flipOneHelp (prev : bool list, curr : bool list, acc : bool list list) : bool list list =
  case curr of
  [] => acc
  | x::xs =>
  if x then flipOneHelp (prev @ [x], xs, acc @ [prev @ false::xs])
  else flipOneHelp (prev @ [x], xs, acc)


(* flipOne: bool list -> bool list list
 * REQUIRES: true
 * ENSURES: flipOne L ==>* An ordered list containing all the ways to flip exactly
 *          one true to false.
 *)
fun flipOne (L : bool list) : bool list list =
    flipOneHelp ([], L, [])




(* TEST CASES *)
(* Write test cases here *)
val [[false,true,true],[true,false,true],[true,true,false]] = flipOne [true, true, true]
val [] = flipOne [false,false,false,false,false]
val [[false]] = flipOne [true]
val [[false,false,true],[true,false,false]] = flipOne [true,false,true]
