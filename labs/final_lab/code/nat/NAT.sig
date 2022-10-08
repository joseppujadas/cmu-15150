signature NAT =
sig
  type nat
  val zero : nat

  val succ : nat -> nat
  val recur : 'a -> (nat -> 'a -> 'a) -> nat -> 'a
  val toInt : nat -> int
  val fromInt : int -> nat
end
