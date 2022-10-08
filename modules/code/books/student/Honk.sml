(* Instantiate MkStudent to create Honk *)
structure Honk :> STUDENT = MkStudent(
    struct
      val count = 1
      val favoriteWord = "pointer"
    end
  )
