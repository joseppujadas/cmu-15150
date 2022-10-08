structure StreamRand : STREAMRAND =
struct
  fun bbs M s =
    let
      fun bbs1 M s = Stream.delay(fn () => bbs2 M s)
      and bbs2 M s = Stream.Cons (s, bbs1 M ((s * s) mod M))
      val q as Stream.Cons (x,y) = Stream.expose(bbs1 M s)
    in
      y
    end
end
