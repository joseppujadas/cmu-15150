(* Write Polly, the 150 fanatic, here! *)
structure Polly :> STUDENT =
struct


  type thoughts = int

  datatype status
    = YES
    | NO
    | MAYBE of thoughts * Book.direction




  val start = 3

  (*fun think : thoughts * (bool * Book.page option) -> status*)
  fun think (0, _)               = YES
    | think (_, (_, NONE))       = NO
    | think (count, (_, SOME p)) =
      if p = "lambda" then MAYBE (count-1, Book.FORWARD)
      else MAYBE (count,Book.FORWARD)

end
