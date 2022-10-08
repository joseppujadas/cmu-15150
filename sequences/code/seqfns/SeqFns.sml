structure SeqFns :> SEQFNS =
struct
  (* exists: ('a -> bool) -> 'a Seq.t -> bool
   * REQUIRES: p is total
   * ENSURES:
   *   if there exists an element x in S such that p x ==> true
   *     then exists p S ==> true
   *     else exists p S ==> false
   *   exists p S has O(|S|) work and O(log|S|) span
   *     if p has O(1) work and span
   *)
  fun exists p xs = (Seq.length (Seq.filter p xs) > 0)

  (* transpose: 'a Seq.t Seq.t -> 'a Seq.t Seq.t
   * REQUIRES: S is a well-formed n x m matrix
   * ENSURES: transpose S ==> S' where
   *   if n = 0 then S' is the "trivial" matrix
   *   otherwise S' is a well-formed m x n matrix
   *   and forall i, j: S[i][j] ~= S'[j][i]
   *  transpose S has O(mn) work
   *  transpose S has O(1) span
   *)
  fun transpose xss = Seq.tabulate (fn i => Seq.tabulate (fn j => Seq.nth (Seq.nth xss j) i) (Seq.length xss)) (Seq.length (Seq.nth xss 0))

end
