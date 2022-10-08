structure BinaryTree :> TREE =
struct

  datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree
  type 'a cmpFxn = 'a * 'a -> order

  exception NotFound

  fun empty () = Empty

  fun insert (cmp : 'a cmpFxn) Empty x = Node (Empty, x, Empty)
    | insert cmp (Node (l, v, r)) x =
        case cmp (x, v) of
          LESS => Node (insert cmp l x, v, r)
        | _    => Node (l, v, insert cmp r x)

  fun remove (cmp : 'a cmpFxn) Empty _ = Empty
    | remove cmp (Node (l, v, r)) x =
        case cmp (x, v) of
          EQUAL => Empty
        | LESS  => Node (remove cmp l x, v, r)
        | _     => Node (l, v, remove cmp r x)

  fun isIn (cmp: 'a cmpFxn) Empty _ = false
    | isIn cmp (Node (l, v, r)) x =
        cmp (x, v) = EQUAL orelse isIn cmp l x orelse isIn cmp r x

  fun find (cmp: 'a cmpFxn) Empty _ = raise NotFound
    | find cmp (Node (l, v, r)) x =
        case cmp (x, v) of
          EQUAL => Node (l, v, r)
        | LESS  => find cmp l x
        | _     => find cmp r x

  fun findRoot Empty = raise NotFound
    | findRoot (Node (l, v, r)) = v

end
