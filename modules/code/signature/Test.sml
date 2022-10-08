structure Test =
  struct
    structure Forest = MkForest (
      structure Tree = BinaryTree
      val lim = 4
    )

    val cmp = Int.compare
    val t0 = foldl (fn (x, t) => Forest.T.insert cmp t x)
                   (Forest.T.empty ())
                   [4, 1, 65, 3, 8, 7, 2]
    val t1 = foldl (fn (x, t) => Forest.T.insert cmp t x)
                   (Forest.T.empty ())
                   [10, 9, 11, 12, 13]

    val 4 = Forest.limit
    val fe : int Forest.forest = Forest.empty ()
    val f1 = Forest.addToForest fe t0
    val f2 = Forest.addToForest f1 t1
    val f3 = Forest.addToTree cmp f2 3 15
               handle Forest.Full => f2
    val f4 = Forest.addToTree cmp f3 9 20
    val _ = SOME (Forest.findRoot cmp f4 20)
               handle Forest.NotInForest => NONE
    val _ = SOME (Forest.removeTree cmp f4 10)
               handle Forest.NotInForest => NONE
  end
