(* f1 : 'a -> 'b -> 'c -> 'a *)
val f1 = raise Fail "f1 unimplemented"

(* f2 : ('a * 'b -> 'c) -> 'a -> 'b -> 'c *)
val f2 = raise Fail "f2 unimplemented"

(* f3 : (int * int -> int) -> int -> int -> int *)
val f3 = raise Fail "f3 unimplemented"

(* f4 : 'a -> ('b -> 'a) -> 'b -> ('a * 'a) *)
val f4 = raise Fail "f4 unimplemented"

(* f5 : ('a * 'b -> ('c -> 'd) -> 'd)
 *       -> 'a option
 *       -> 'b option
 *       -> ('c option -> 'd)
 *       -> 'd
 *)
val f5 = raise Fail "f5 unimplemented"
