datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree
datatype 'a rose = Rose of 'a * 'a rose list

fun treeToRose (Empty : 'a tree) : 'a rose option = NONE
  | treeToRose (Node(L,x,R)) =
  let
    val (lRose,rRose) = (treeToRose L, treeToRose R)
  in
    case (lRose, rRose) of
      (NONE, NONE) => SOME (Rose(x,[]))
    | (SOME l, NONE) => SOME (Rose(x, [l]))
    | (NONE, SOME r) => SOME (Rose(x, [r]))
    | (SOME l, SOME r) => SOME (Rose(x, [l,r]))
  end


val SOME (Rose (2, [Rose (1, []), Rose (3, [])])) =
  treeToRose (Node (Node (Empty, 1, Empty), 2, Node (Empty, 3, Empty)))


fun roseMap (f : 'a -> 'b) (Rose(x,rs)) = Rose(f x, map (roseMap f) rs)



val Rose (4, [Rose (2, []), Rose (6, [])]) =
  roseMap (fn x => x * 2) (Rose (2, [Rose (1, []), Rose (3, [])]))


fun roseFoldl (f : 'a * 'b -> 'b) (z : 'b) (Rose (x, []) : 'a rose) : 'b = f (x, z)
  | roseFoldl (f : 'a * 'b -> 'b) (z : 'b) (Rose (x, rs) : 'a rose) : 'b =
  let
    val foldfun = fn (r, z) => roseFoldl f z r
    val xacc = f(x,z)
  in
    foldl foldfun xacc rs
  end


val "z213" = roseFoldl (fn (x, ac) => ac ^ Int.toString x) "z"
  (Rose (2, [Rose (1, []), Rose (3, [])]))


fun roseFoldr (f : 'a * 'b -> 'b) (z : 'b) (Rose (x, []) : 'a rose) : 'b = f (x, z)
  | roseFoldr (f : 'a * 'b -> 'b) (z : 'b) (Rose (x, rs) : 'a rose) : 'b =
  let
    val foldfun = fn (r, z) => roseFoldr f z r
  in
    f(x, foldr foldfun z rs)
  end



val "z312" = roseFoldr (fn (x, ac) => ac ^ Int.toString x) "z"
  (Rose (2, [Rose (1, []), Rose (3, [])]))
