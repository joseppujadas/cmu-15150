(* Implement your Book structure here! *)
structure Book :> BOOK =
struct
  type page = string
  datatype direction = FORWARD | BACKWARD

  exception OutOfBounds

  type book = page list * page list

  fun bind (L : page list) = (L,[])

  fun unbind (F,B) = rev B @ F

  fun currentPage ([],[]) = (true,NONE)
    | currentPage (x::xs, []) = (true, SOME x)
    | currentPage (x::xs, y::ys) = (false, SOME x)
    | currentPage ([], y::ys) = (false, NONE)

  fun flip FORWARD ([],_) = raise OutOfBounds
    | flip BACKWARD (_, []) = raise OutOfBounds
    | flip FORWARD (x::xs, B) = (xs, x::B)
    | flip BACKWARD (F, x::xs) = (x::F, xs)


end
