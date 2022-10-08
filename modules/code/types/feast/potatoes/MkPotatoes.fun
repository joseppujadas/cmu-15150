signature PRINTABLE =
  sig
    type t
    val toString : t -> string
  end

functor MkPotatoes (
  val prefix : string
  structure Topping : PRINTABLE
) =
  struct
    type t = bool * Topping.t
    val show = fn (hot, top) =>
      (if hot then "hot" else "cold") ^ " "
        ^ prefix ^ " potatoes with " ^ Topping.toString top
  end
