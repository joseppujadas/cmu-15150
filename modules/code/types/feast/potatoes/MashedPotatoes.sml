structure Marshmallow : PRINTABLE =
  struct
    type t = bool
    val toString = fn
      false => "burnt marshmallows"
    | true  => "toasted marshmallows"
  end

structure MashedPotatoes = MkPotatoes (
  val prefix = "mashed"
  structure Topping = Marshmallow
)

val x = MashedPotatoes.show (true, false)
