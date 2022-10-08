datatype regexp = Char of char
                | One
                | Zero
                | Times of regexp * regexp
               | Plus of regexp * regexp
                | Star of regexp

fun isPlus (Plus _) = true
  | isPlus _        = false

fun r_foldl f z (Times(r1,r2)) = r_foldl f (r_foldl f z r1) r2
  | r_foldl f z (Plus(r1,r2))  = r_foldl f (r_foldl f z r1) r2
  | r_foldl f z (Star (r))     = r_foldl f z r
  | r_foldl f z R              = f(R,z)

fun gather (R as Plus (r1, r2)) = r_foldl R
  | gather other = [other]

val combine = raise Fail "Unimplemented"

(* computes the list product *)
fun prod ([], _) = []
  | prod (_, []) = []
  | prod (x::xs, ys) = List.map (fn y => (x, y)) ys @ prod (xs, ys)

fun dnf R = raise Fail "Unimplemented"

val test1 =
  Plus (
    Plus (
      Plus (
        Char #"a",
        One
      ),
      Zero),
    Plus (
      Times (
        Char #"a",
        Zero
      ),
      Plus (
        One,
        Char #"x"
      )
    )
  )
