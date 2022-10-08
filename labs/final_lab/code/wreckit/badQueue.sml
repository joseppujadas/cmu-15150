(* Q*bert wrote this implementation instead. Since ListQueueOpaque is opaquely
 * ascribed, it must be safe right? *)
signature QUEUE =
sig
  type 'a t = 'a list

  val emp : 'a t
  val ins : 'a t * 'a -> 'a t
  val deq : 'a t -> ('a t * 'a) option
end

structure ListQueueOpaque :> QUEUE =
struct
  type 'a t = 'a list

  val emp = []

  fun ins (Q, x) = Q @ [x]

  fun deq [] = NONE
    | deq (x::xs) = SOME (xs, x)
end
