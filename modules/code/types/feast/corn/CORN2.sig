signature CORN2 =
  sig
    type kernel
    type t = kernel * int
    val color : t -> string
  end
