signature CORN1 =
  sig
    datatype kernel = White | Yellow | Red
    type t
    val color : t -> string
    val eat : kernel * int -> t option
  end
