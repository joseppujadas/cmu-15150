functor MkTest (Book : BOOK) :> TEST =
struct
  exception Error

  val test = fn () =>
    let
      val B0 = Book.bind []

      val () = (
        case Book.currentPage B0 of
          (true, NONE) => ()
        | _ => raise Error
      )

      val () = (
        case Book.unbind B0 of
          [] => ()
        | _ => raise Error
      )

      (* testing for unhandled Book.OutOfBounds exception *)
      val () = (let val _ = Book.flip Book.FORWARD B0 in raise Error end) handle Book.OutOfBounds => ()

      val B1 = Book.bind ["Is","this","the","real","life?"]

      val () = (
        case Book.currentPage B1 of
          (true, SOME "Is") => ()
        | _ => raise Error
      )

      val () = (
        case Book.unbind B1 of
          ["Is","this","the","real","life?"] => ()
        | _ => raise Error
        )

      val () = (let val _ = Book.flip Book.BACKWARD B1 in raise Error end) handle Book.OutOfBounds => ()

      val B1 = Book.flip Book.FORWARD B1

      val () = (
        case Book.currentPage B1 of
          (false, SOME "this") => ()
        | _ => raise Error
      )

      val () = (
        case Book.unbind B1 of
          ["Is","this","the","real","life?"] => ()
        | _ => raise Error
        )

      val B1 = Book.flip Book.BACKWARD B1

      val () = (
        case Book.currentPage B1 of
          (true, SOME "Is") => ()
        | _ => raise Error
      )

      val () = (
        case Book.unbind B1 of
          ["Is","this","the","real","life?"] => ()
        | _ => raise Error
        )

      val B1 = Book.flip Book.FORWARD B1

      val () = (
        case Book.currentPage B1 of
          (false, SOME "this") => ()
        | _ => raise Error
      )

      val () = (
        case Book.unbind B1 of
          ["Is","this","the","real","life?"] => ()
        | _ => raise Error
        )

      val B1 = Book.flip Book.FORWARD B1

      val () = (
        case Book.currentPage B1 of
          (false, SOME "the") => ()
        | _ => raise Error
      )

      val () = (
        case Book.unbind B1 of
          ["Is","this","the","real","life?"] => ()
        | _ => raise Error
        )

      val B1 = Book.flip Book.FORWARD B1

      val () = (
        case Book.currentPage B1 of
          (false, SOME "real") => ()
        | _ => raise Error
      )

      val () = (
        case Book.unbind B1 of
          ["Is","this","the","real","life?"] => ()
        | _ => raise Error
        )

      val B1 = Book.flip Book.FORWARD B1

      val () = (
        case Book.currentPage B1 of
          (false, SOME "life?") => ()
        | _ => raise Error
      )

      val () = (
        case Book.unbind B1 of
          ["Is","this","the","real","life?"] => ()
        | _ => raise Error
        )

      val B1 = Book.flip Book.FORWARD B1

      val () = (
        case Book.currentPage B1 of
          (false,NONE) => ()
        | _ => raise Error
      )

      val () = (
        case Book.unbind B1 of
          ["Is","this","the","real","life?"] => ()
        | _ => raise Error
        )

      val () = (let val _ = Book.flip Book.FORWARD B1 in raise Error end) handle Book.OutOfBounds => ()

      val B1 = Book.flip Book.BACKWARD B1

      val () = (
        case Book.currentPage B1 of
          (false, SOME "life?") => ()
        | _ => raise Error
      )

      val () = (
        case Book.unbind B1 of
          ["Is","this","the","real","life?"] => ()
        | _ => raise Error
        )

      val B1 = Book.flip Book.BACKWARD B1

      val () = (
        case Book.currentPage B1 of
          (false, SOME "real") => ()
        | _ => raise Error
      )

      val () = (
        case Book.unbind B1 of
          ["Is","this","the","real","life?"] => ()
        | _ => raise Error
        )

      val B1 = Book.flip Book.BACKWARD B1

      val () = (
        case Book.currentPage B1 of
          (false, SOME "the") => ()
        | _ => raise Error
      )

      val () = (
        case Book.unbind B1 of
          ["Is","this","the","real","life?"] => ()
        | _ => raise Error
        )

      val B1 = Book.flip Book.BACKWARD B1

      val () = (
        case Book.currentPage B1 of
          (false, SOME "this") => ()
        | _ => raise Error
      )

      val () = (
        case Book.unbind B1 of
          ["Is","this","the","real","life?"] => ()
        | _ => raise Error
        )

      val B1 = Book.flip Book.BACKWARD B1

      val () = (
        case Book.currentPage B1 of
          (true, SOME "Is") => ()
        | _ => raise Error
      )

      val () = (let val _ = Book.flip Book.BACKWARD B1 in raise Error end) handle Book.OutOfBounds => ()

      val () = (
        case Book.unbind B1 of
          ["Is","this","the","real","life?"] => ()
        | _ => raise Error
        )
    in
      ()
    end
end
