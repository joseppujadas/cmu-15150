
signature SET =
sig
  type t
  type elem

  val emp : t
  val ins : t * elem -> t
  val lookup : t * elem -> bool
end

signature NAT =
sig
  type t
  exception Zero

  val zero : t
  val succ : t -> t
  val pred : t -> t
  val is_zero : t -> bool

  val toInt : t -> int
end

signature LENSET =
sig
  type t
  type elem

  val emp : t
  val ins : t * elem -> t
  val lookup : t * elem -> bool

  val len : t -> int
end

(* INVARIANT: for S : LenSet.t, len S is evaluates to
*             the number of elements inserted into S
*)
functor LenSet (structure Nat : NAT
                structure Set : SET) : LENSET =
struct
  type t = Nat.t * Set.t
  type elem = Set.elem

  val emp = (Nat.zero, Set.emp)

  fun ins ((n, s), v) = (Nat.succ n, Set.ins (s, v))

  fun lookup ((_, s), v) = Set.lookup (s, v)

  fun len (n, _) = Nat.toInt n
end
