signature FOREST =
  sig

    type 'a forest

    structure T : TREE
    val limit : int



    exception Full
    exception NotInForest

    val empty : unit -> 'a forest
    val addToForest : 'a forest -> 'a T.tree -> 'a forest
    val addToTree : ('a * 'a -> order) -> 'a forest -> 'a -> 'a -> 'a forest
    val findRoot  : ('a * 'a -> order) -> 'a forest -> 'a -> 'a
    val removeTree : ('a * 'a -> order) -> 'a forest -> 'a -> 'a forest

  end
