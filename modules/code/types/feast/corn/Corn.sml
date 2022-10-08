structure Corn : CORN2 =
  struct
    datatype kernel = White | Yellow | Red  (* "flint corn" *)
    type t = kernel * int         (* dominant color, number *)

    val color = fn
      (White, _) => "White"
    | (Yellow, _) => "Yellow"
    | (Red, _) => "Red"

    val eat = fn
      (_, 0) => NONE
    | (k, i) => SOME (k, i-1)
  end
