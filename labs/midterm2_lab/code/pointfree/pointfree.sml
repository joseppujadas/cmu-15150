(* Point-Free Programming *)
val co = fn a => fn b => a o b

(* big_map : unit -> ('a -> 'b) -> 'a list list -> 'b list list*)
val big_map = fn () => map o map

(* TEST CASES *)
val [[2,3],[],[4]] = big_map () (fn x => x + 1) [[1,2],[],[3]]

(* sum_all : int list list -> int *)
val sum_all = (foldl (op +) 0) o (map (foldl (op +) 0))

(* TEST CASES *)
val 6 = sum_all [[1,2],[],[3]]

(* bind : unit -> ('a -> 'b list) -> 'a list -> 'b list *)
val bind = fn () => (List.concat) o map

(* TEST CASES *)
(* val [1,2,2,3,3,4] = bind () (fn x => [x,x+1]) [1,2,3] *)

(* map_long : unit -> ('a -> 'b) -> ('b -> 'b) list -> 'a list -> 'b list *)
val map_long = ()

(* TEST CASES *)
(*
   val ["1-beep-boop", "2-beep-boop", "3-beep-boop"] =
      map_long () Int.toString
       [
        (fn x => x ^ "-beep"),
        (fn x => x ^ "-boop")
       ]
       [1,2,3]
 *)
