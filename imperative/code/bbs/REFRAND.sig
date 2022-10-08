signature REFRAND = sig
  type randstate (* abstract *)
  val init : int -> int -> randstate
  val next : randstate -> int
end
