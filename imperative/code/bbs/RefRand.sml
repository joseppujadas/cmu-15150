structure RefRand : REFRAND =
struct
  (* Change me! *)
  type randstate = (int * int) ref

  fun init m s = ref (m, s)

  fun next R =
    let
      val (m,r) = !R
      val new = (r*r) mod m
      val () = R := (m,new)
    in
      new
    end
end
