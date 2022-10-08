structure MSort =
  struct
    (* halves : 'a Seq.t -> 'a Seq.t * 'a Seq.t
     * REQUIRES: true
     * ENSURES: halves S = (S1,S2),
     * where S1 and S2 have approximately the same length, and append (S1,S2) = S
     *)
    fun halves S = (Seq.take S (Seq.length S div 2), Seq.drop S (Seq.length S div 2))

    (* msort : ('a * 'a -> order) -> 'a Seq.t -> 'a Seq.t
      * REQUIRES: cmp total
      * ENSURES: msort cmp S = S',
      * where S' is a cmp-sorted permutation of S
      *)
    fun msort cmp S =
      case (Seq.length S) of
       0 => S
     | 1 => S
     | _ =>
      let
        val (L,R) = halves S
      in
        Seq.merge cmp (msort cmp L, msort cmp R)
      end
  end
