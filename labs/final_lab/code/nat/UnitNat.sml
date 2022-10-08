structure UnitNat :> NAT =
struct
  type nat = unit list

  val zero : nat = []

  fun succ n = () :: n

  fun recur z f e = raise Fail "epe"

  fun toInt n = length n

  fun fromInt n = List.tabulate (n, fn i => ())
end
