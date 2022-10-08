(* Write the Reset structure here! *)
structure Reset :> STUDENT =
struct
  type thoughts = bool

  datatype status
    = YES
    | NO
    | MAYBE of thoughts * Book.direction

  val start = false

  fun think (_,(true, _)) = YES
    | think (true, _)     = YES
    | think (t,(false,_)) = MAYBE (false,Book.BACKWARD)

end
