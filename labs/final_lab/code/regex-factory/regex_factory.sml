datatype regexp =   Zero
                  | One
                  | Char of char
                  | Times of regexp * regexp
                  | Plus of regexp * regexp
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


fun rev r = raise Fail "unimpleemented"

fun allPrefix r = raise Fail "unimplemented"

val allSuffix = fn () => raise Fail "unimplemented"

val allSubstrings = fn () => raise Fail "unimplemented"


