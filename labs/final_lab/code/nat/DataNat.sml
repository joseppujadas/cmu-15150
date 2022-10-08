structure DataNat :> NAT =
struct
  datatype nat = ZERO | SUCC of nat

  val zero = ZERO

  fun succ n = SUCC(n)

  fun recur z f ZERO    = z
    | recur z f (SUCC(n)) = f (SUCC(n)) (recur z f n)

  fun toInt ZERO = 0
    | toInt (SUCC(n)) = 1 + (toInt n)

  fun fromInt 0 = ZERO
    | fromInt n = SUCC(fromInt(n-1))
end
