datatype nat = Zero | Succ of nat

(* toNat : int -> nat
 * REQUIRES: d >= 0
 * ENSURES: toNat x ==> n, where x is the integer representation of n
 *)
fun toNat (0 : int) : nat = Zero
  | toNat (x : int) : nat = Succ (toNat (x - 1))

(* toInt : nat -> int
 * REQUIRES: true
 * ENSURES: toInt n ==> x, where x is the integer representation of n
 *)
fun toInt (Zero : nat) : int = 0
  | toInt ((Succ n) : nat) = 1 + (toInt n)

(* natAdd : nat * nat -> nat
 * REQUIRES: true
 * ENSURES: natAdd (n, m) = toNat (toInt n + toInt m)
 *)
fun natAdd (Zero : nat, m : nat) : nat = m
  | natAdd ((Succ n) : nat, m : nat) = Succ (natAdd (n, m))

(* natMult : nat * nat -> nat
 * REQUIRES: true
 * ENSURES: natMult (n, m) = toNat (toInt n * toInt m)
 *)
fun natMult (n : nat, m : nat) : nat = raise Fail "Unimplemented"
