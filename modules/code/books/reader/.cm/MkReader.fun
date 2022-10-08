(* Your MkReader functor, taking in a Student structure, goes here. *)
functor MkReader (Stu : STUDENT) :> READER =
  struct
    structure Student = Stu

    fun read B =
      let
        fun r (b : Book.book) (t : Student.thoughts) =
          case Student.think(t, (Book.currentPage b)) of
            Student.NO => false
          | Student.YES => true
          | Student.MAYBE(tho,dir) => r (Book.flip dir b) tho
      in
        r B Student.start
      end
  end
