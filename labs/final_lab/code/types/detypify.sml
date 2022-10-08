val f1 = fn x => fn y => fn z => (x z) (y z)

val f2 = fn x => fn y => x

(* (’a -> ’b) -> (unit -> ’b) -> ’a option -> ’b *)
val f3 = fn f => fn g => fn x =>
  (case x of
    SOME y => f y
  | _      => g ())


val f4 = fn f => fn x => fn y => f(x,y)

val f5 = fn f => fn x => fn y => f(x+1,y+1) + 1

val f6 = fn x => fn f => fn y =>
  if true then (f y, x) else (x,x)

val f7 = fn f => fn x => fn g =>
  case x of
    SOME y => 
  | _      =>
