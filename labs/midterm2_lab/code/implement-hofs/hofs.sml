(* unfold : ('a -> ('b * 'a) option) -> 'a -> 'b list
 * REQUIRES: true
 * ENSURES: unfold f b ==>* L', the list of elements “generated” by repeatedly
 *          applying f, stopping once the application evaluates to NONE
 *)
fun unfold f b =
  case f b of
    NONE => []
  | SOME (x,y) => x::(unfold f y)


fun stringSub x =
  if x = 0 then NONE
  else SOME (Int.toString x, x - 1)

(* scanl: ('a * 'b -> 'b) -> 'b -> 'a list -> 'b list
 * REQUIRES: true
 * ENSURES: scanl f z [x1, x2, ..., xn] ==>*
 *          [z, f(x1, z), f(x2, f(x1, z)), ..., f(xn, ..., f(x2, f(x1, n))...)]
 *)
(* fun scanl (f : 'a * 'b -> 'b) (z : 'b) (L: 'a list) =
  let
    val acc = []
  in
    foldl (fn (x,y) => )
  end *)

(* partition: ('a -> bool) -> 'a list -> 'a list * 'a list
 * REQUIRES: true
 * ENSURES: partition p L ==>* (R1,R2) such that
 *          \code{R1} contains all elements of \code{L} that satisfy \code{p}
 *          and \code{R2} contains all other elements
 *)

(* This implementation should not use HOFs. *)
fun partition (p : 'a -> bool) ([] : 'a list) : 'a list  * 'a list = ([],[])
  | partition (p) (x::xs) =
    let
      val (l,r) = partition p xs
    in
      if p x then (x::l,r) else (l,x::r)
    end

(* This implementation should use HOFs. *)
fun partition' p L = foldl (fn (x, (l,r)) => if p x then (x::l,r) else (l,x::r)) ([],[]) L
