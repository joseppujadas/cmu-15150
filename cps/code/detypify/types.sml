(* task1 : ('a -> 'b) -> 'a -> 'b *)
fun task1 f x = f x

(* task2 : ('a -> bool) -> 'a -> ('a -> 'b) -> (unit -> 'b) -> 'b *)
fun task2 p x sc fc = if p x then sc x else fc ()

(* task3 : ('a -> 'b) -> 'a list -> ('b list -> 'c) -> 'c *)
fun task3 f l k = k (map f l)
