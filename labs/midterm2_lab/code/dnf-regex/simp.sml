datatype regexp = Char of char
                | One
                | Zero
                | Times of regexp * regexp
                | Plus of regexp * regexp
                | Star of regexp

type simp = regexp -> regexp

fun simp1 (Times (One, r)) = r
  | simp1 (Times (r, One)) = r
  | simp1 other = other

fun simp2 (Times (Zero, _)) = Zero
  | simp2 (Times (_, Zero)) = Zero
  | simp2 other = other

fun simp3 (Plus (Zero, r)) = r
  | simp3 (Plus (r, Zero)) = r
  | simp3 other = other

fun simp4 (Star Zero) = One
  | simp4 (Star One)  = One
  | simp4 other = other

fun simp5 (Times (r1, Plus (r2, r3))) = Plus(Times(r1,r2),Times(r1,r3))
  | simp5 (Times (Plus (r1, r2), r3)) = Plus(Times(r1,r3),Times(r2,r3))
  | simp5 other = other

val simps = [simp1, simp2, simp3, simp4, simp5]

val unite = fn () => foldl (op o) (fn x => x)

fun r_apply f (Times(r1,r2)) = f (Times(r_apply f r1, r_apply f r2))
  | r_apply f (Plus(r1,r2)) = f (Plus(r_apply f r1, r_apply f r2))
  | r_apply f (Star(r)) = f (Star (r_apply f r))
  | r_apply f R = f R


val simplify = r_apply (unite () simps)
