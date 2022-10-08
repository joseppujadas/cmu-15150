datatype regexp =
    Zero
  | One
  | Char of char
  | Plus of regexp * regexp
  | Times of regexp * regexp
  | Star of regexp

fun match (r : regexp) (cs : char list) (k : char list -> bool) : bool =
    case r of
      Zero   => false
    | One    => k cs
    | Char c => (
        case cs of
          nil       => false
        | c' :: cs' => c' = c andalso k cs'
      )
    | Plus  (r1,r2) => match r1 cs k orelse match r2 cs k
    | Times (r1,r2) => match r1 cs (fn cs' => match r2 cs' k)
    | Star r => (
        let
          fun matchrstar cs' = k cs' orelse match r cs' matchrstar
        in
          matchrstar cs
        end
      )

fun accept r s = match r (String.explode s) List.null


val fun_times : regexp = Times(Char #"f", Times(Char #"u", Char #"n"))

val anything_goes : regexp = Times(Star(Char #"x"), Star(Char #"y"))

val alternating : regexp = Star (Times(Char #"x", Char #"y"))

val paired : regexp = Star (Plus(Times(Char #"x", Char #"y"), Times((Char #"y", Char #"x"))))

val yay : regexp = Times(Times(Char #"y", Times(Char #"a", Star(Char #"a"))), Char #"y")

val no : regexp = Times(Times(Char #"n", Char #"o"), Star(Plus(Char #"o", Char #"O")))
