datatype 'a road = Empty | Fork of 'a road * 'a * 'a road

fun roadsolve (r : 'a road)
              (p : 'a list -> ('a list -> 'c) -> (unit -> 'c) -> 'c)
              (sc : 'a list -> 'c)
              (fc : unit -> 'c) : 'c =
    case r of
      _ => raise Fail "Unimplemented"

fun roadsolvel (p : 'b -> bool)
               (f : 'a * 'b -> 'b)
               (z : 'b)
               (r : 'a road)
               (sc : 'a list -> 'c)
               (fc : unit -> 'c) : 'c =
    case r of
      _ => raise Fail "Unimplemented"

val test1 =
  Fork(
    Fork(
      Fork (Empty, 4, Empty),
      2,
      Fork (Empty, 5, Empty)),
    1,
    Fork(
      Fork (Empty, 6, Empty),
      3,
      Fork (Empty, 7, Empty)
    )
  )

val p1 = fn L => fn sc => fn fc => if foldr (op +) 0 L = 10 then sc L else fc ()
val SOME [1, 3, 6] = roadsolve test1 p1 SOME (fn () => NONE)
val SOME [1, 3, 6] = roadsolvel (fn x => x = 10) (op +) 0 test1 SOME (fn () => NONE)
