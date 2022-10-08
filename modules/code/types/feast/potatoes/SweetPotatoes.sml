functor MkAnd (
  structure A : PRINTABLE
  structure B : PRINTABLE
) : PRINTABLE =
  struct
    type t = A.t * B.t
    val toString = fn (a, b) =>
      A.toString a ^ " and " ^ B.toString b
  end

structure Marshmallow : PRINTABLE =
  struct
    type t = bool
    val toString = fn
      false => "burnt marshmallows"
    | true  => "toasted marshmallows"
  end

structure Pineapple :> PRINTABLE =
  struct
    type t = unit
    val toString = fn () => "pineapple"
  end

structure SweetPotatoes = MkPotatoes (
  val prefix = "sweet"
  structure Topping = MkAnd (
    structure A = Marshmallow
    structure B = Pineapple
  )
)

val x = SweetPotatoes.show (false, (true, ()))
