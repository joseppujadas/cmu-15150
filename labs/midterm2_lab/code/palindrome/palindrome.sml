(* rev_acc: (''a list -> bool) -> ''a list -> ''a list -> bool
 * REQUIRES: p is total
 * ENSURES: rev_acc p L1 L2 = true iff there is some L2', s such that List.rev L1 = L2' and L2' @ s = L2 and p L2 = true
 *          rev_acc p L1 l2 = false otherwise
 *)
fun rev_acc (p : ''a list -> bool) ([] : ''a list) (L2 : ''a list) : bool = p L2
  | rev_acc p (x::xs) L2 =
    let
      fun p' [] = false
        | p' (y::ys) = x = y andalso p ys
    in
      rev_acc p' xs L2
    end

(* palindromeV1 : (char list -> bool) -> char list -> bool
 * REQUIRES: p is total
 * ENSURES: palindromeV1 p L = true iff there are some L1, L2 such that L1@L2 = L and L1 is a palindrome and p L2 = true
 *          palindromeV1 p L = false otherwise *)
fun palindromeV1 (p : char list -> bool) (L1 : char list) : bool =
  raise Fail "Unimplemented"

(* TEST CASES *)
(*
val p = List.null
val true = palindromeV1 p []
val false = palindromeV1 p [#"a", #"b"]
val true = palindromeV1 p [#"a", #"b", #"a"]
val true = palindromeV1 p [#"a", #"b", #"b", #"a"]
*)

(* palindromeV2: (char list -> (char list -> 'a) -> (unit -> 'a) -> 'a) ->
                char list -> (char list -> 'a) -> (unit -> 'a) -> 'a
 * REQUIRES: p is total
 * ENSURES: palindromeV2 p L sc fc = sc L iff there are some L1, L2 such that L1@L2 = L and L1 is a palindrome and p L2 = true
 *          palindromeV2 p L sc fc = fc () *)
fun palindromeV2
    (p : char list -> (char list -> 'a) -> (unit -> 'a) -> 'a)
    (L : char list)
    (sc : char list -> 'a)
    (fc : unit -> 'a)
    : 'a =
      raise Fail "Unimplemented"

(* TEST CASES *)
(*
    val p = (fn L => fn sc => fn fc => case L of [] => sc [] | _ => fc ())
    val true = palindromeV2 p [] (fn _ => true) (fn () => false)
    val false = palindromeV2 p [#"a", #"b"] (fn _ => true) (fn () => false)
    val true = palindromeV2 p [#"a", #"b", #"b", #"a"] (fn _ => true) (fn () => false)
    val [#"a", #"c", #"b", #"c", #"a"] = palindromeV2 p [#"a", #"c", #"b", #"c", #"a"] (fn x => x) (fn () => [])
    val [#"a", #"b", #"b", #"a"] = palindromeV2 p [#"a", #"b", #"b", #"a"] (fn x => x) (fn () => [])
 *)
