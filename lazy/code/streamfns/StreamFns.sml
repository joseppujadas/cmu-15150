structure StreamFns :> STREAMFNS =
struct
  structure S = Stream

  (* concat: 'a Stream.stream Stream.stream -> 'a Stream.stream
   * REQUIRES: S is productive
   * ENSURES:
   *     - concat S = S', a stream containing the elements of S concatenated
   *       together.
   *     - concat S is maximally lazy (evaluating concat S never exposes any
   *       elements of S).
   *)
  fun concat x = S.delay (fn () => concat' (S.expose x))
  and concat' (S.Empty)      = S.Empty
    | concat' (S.Cons(y,x2)) = S.expose(S.append(y,concat(x2)))


  (* cycle: 'a Stream.stream -> 'a Stream.stream
   * REQUIRES: S is productive
   * ENSURES:
   *     - cycle S is productive
   *     - cycle S = S', a stream formed by infinitely repeating the elements of S
   *     - if S = Stream.empty, then cycle S = Stream.empty
   *     - cycle S is maximally lazy (evaluating cycle S never exposes any
   *       elements of S).
   *)



    fun cycle x =
     let

       fun cycle' y = Stream.delay (fn () =>
         (case (S.expose y) of
                S.Empty      => S.expose(cycle' x)
              | S.Cons(z,zz) => S.Cons(z, cycle' zz)))
     in
       Stream.delay (fn () =>
        (case (S.expose x) of
            S.Empty      => S.Empty
          | S.Cons(z,zz) => S.Cons(z,(cycle' zz))))
     end




  (* interleave: 'a Stream.stream -> 'a Stream.stream -> 'a Stream.stream
   * REQUIRES: S1 and S2 are productive
   * ENSURES:
   *     - interleave S1 S2 is productive
   *     - interleave S1 S2 = S, where S contains alternating elements from S1
   *       and S2, starting with the first element of S1
   *     - If one stream ends early, the rest of the other stream is appended in its entirety
   *     - interleave S1 S2 is maximally lazy (evaluating interleave S1 S2
   *       never exposes any elements of S1 or S2).
   *)
  fun interleave S1 S2 = S.delay(fn () => interleave' (S.expose S1) S2)
  and interleave' (S.Empty)      S2 = S.expose S2
    | interleave' (S.Cons(x,S1)) S2 = S.Cons(x,interleave S2 S1)

  (* double: 'a Stream.stream -> 'a Stream.stream
   * REQUIRES: S is productive
   * ENSURES:
   *     - double S is productive
   *     - double S = S', a stream containing the same elements as S, but with each
   *       element appearing twice in a row
   *     - double S is maximally lazy (evaluating double S never exposes any
   *       elements of S).
   *)
   fun double s = interleave s s

end
