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



type command_sequence = string  (* command sequence *)

(* example *)
val cmu_to_waterfront : command_sequence = "ES"

val sqh_loop_1 : command_sequence = "SN"  (* REPLACE ME *)
val sqh_loop_2 : command_sequence = "WE"  (* REPLACE ME *)

val cmu_to_airport_1 : command_sequence = "W"  (* REPLACE ME *)
val cmu_to_airport_2 : command_sequence = "ESW"  (* REPLACE ME *)

local
  val N = Char #"N"
  val S = Char #"S"
  val E = Char #"E"
  val W = Char #"W"
  val op + = Plus
  val op * = Times
in
  val sqh_to_sqh : regexp = Star ((W * E) + (S * N))

  val cmu_to_airport : regexp = (One + (E * sqh_to_sqh * (S + W))) * W
end
