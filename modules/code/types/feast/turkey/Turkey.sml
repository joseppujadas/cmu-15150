signature TURKEY =
  sig
    type t
    val Boil : t
    val Bake : t
    val show : t -> string
  end

structure Turkey : TURKEY =
  struct
    datatype t = Boil | Bake | Stuff
    val show = fn Boil => "Boiled Turkey"
                | Bake => "Baked Turkey"
                | Stuff => "Stuffed Turkey"
  end

val t1 = Turkey.Bake
val x = case t1 of
            Turkey.Boil => Turkey.show t1
          | _ => "Not Boiled Turkey"
