fun fuse (Var x) = Var x
  | fuse (Int n) = Int n
  | fuse (Add (a,b)) = Add (fuse a, fuse b)
  | fuse (Mul (a,b)) = (
      case (vars a,vars b) of
        ([],[]) => Int (eval ([],a) * eval ([],b))
      | _       => Mul (fuse a,fuse b)
    )
  | fuse (Not a) = Not (fuse a)
  | fuse (IfThenElse (i,t,e)) = IfThenElse (fuse i,fuse t,fuse e)

val Add (Var "x",Int 6) = fuse (Add (Var "x", Mul (Int 2,Int 3)))
