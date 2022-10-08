(* Your functor MkStudent goes here. *)
functor MkStudent(
  val count : int
  val favoriteWord : string
  )  :> STUDENT =
struct
  type thoughts = int

  datatype status
    = YES
    | NO
    | MAYBE of thoughts * Book.direction




  val start = count

  (*fun think : thoughts * (bool * Book.page option) -> status*)
  fun think (0, _)               = YES
    | think (_, (_, NONE))       = NO
    | think (c, (_, SOME p)) =
      if p = favoriteWord then MAYBE (c-1, Book.FORWARD)
      else MAYBE (c,Book.FORWARD)
end
