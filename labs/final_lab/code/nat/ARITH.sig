signature ARITH =
sig
  structure Nat : NAT

  val add : Nat.nat -> Nat.nat -> Nat.nat
  val mult : Nat.nat -> Nat.nat -> Nat.nat
  val fact : Nat.nat -> Nat.nat
end
