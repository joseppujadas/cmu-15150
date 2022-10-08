structure FnSet :> BASIC_INTSET =
struct
  type t = int -> bool

  val emp = fn _ => false
  fun ins s i = (fn x => x = i orelse s x)
  val mem = fn s => s
end

structure ListSet :> BASIC_INTSET =
struct
  type t = int list

  val emp = []
  fun ins s i = i :: s
  fun mem s i = List.exists (fn x => x = i) s
end
