functor MkForest (
  structure Tree : TREE
  val lim : int
) :> FOREST =
struct

  type 'a forest = 'a Tree.tree list * int

  structure T = Tree

  exception Full
  exception NotInForest
  exception Empty

  val limit = lim

  fun empty () = ([], 0)

  fun addToForest (ts, i) tree =
    if i >= lim then raise Full else (tree::ts, i + 1)

  fun accForest g f [] key x = raise Empty
    | accForest g f (t::ts) key x =
        if g t key then (f t x)::ts else t::(accForest g f ts key x)

  fun iterateForest g f [] x = raise Empty
    | iterateForest g f (t::ts) x =
        if g t x then f t x else iterateForest g f ts x

  fun addToTree cmp ([], _) key x = raise NotInForest
    | addToTree cmp (ts, i) key x =
        (let
          val nts = accForest (T.isIn cmp) (T.insert cmp) ts key x
        in
          (nts, i)
        end) handle Empty => raise NotInForest

  fun getRoot t x = T.findRoot t

  fun findRoot cmp ([], _) _ = raise NotInForest
    | findRoot cmp (ts, i) x =
        (iterateForest (T.isIn cmp) getRoot ts x)
          handle Empty => raise NotInForest
               | T.NotFound => raise NotInForest

  fun removeTree cmp ([], _) _ = raise NotInForest
    | removeTree cmp (ts, i) x =
      (let
          val nts = accForest (T.isIn cmp) (T.remove cmp) ts x x
        in
          (nts, i)
        end) handle Empty => raise NotInForest

end
