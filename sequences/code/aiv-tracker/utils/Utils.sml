structure Utils :> UTILS =
struct
  (* collect: ('a * 'a -> order) -> 'a Seq.t -> 'a Seq.t Seq.t
   * REQUIRES:
   *    cmp is "well-behaved"
   *    cmp is total
   *    cmp has O(1) work/span
   * ENSURES: collect cmp S ==> C where
   *    Every element in S appears in some sequence of C
   *    If you sum the lengths of the sequences in C, you get the length of S
   *    Elements in each sequence in C are cmp-EQUAL
   *    Elements in earlier sequences in C are cmp-LESS than elements in
   *      later sequences in C
   *    No sequence in C is empty
   *    collect cmp S has O(|S|log|S|) work
   *    collect cmp S has O(log^2|S|) span
   *)


  fun collect cmp S = (*All work/span dominated by the sort*)
  let
    val sorted = Seq.sort cmp S (*O(|S|log|S|) work and O(log^2|S|) span*)

    val enumed = Seq.enum sorted (*O(|S| work, O(1) span)*)

    fun mapFun ((i,x)) =  (*O(1) map function*)
      if (i = 0) then i
      else
      let
        val (j,y) = Seq.nth enumed (i-1)  (*O(1) work/span*)
      in
        if cmp (x,y) = EQUAL then ~1 else i
      end

    val lengths = Seq.map mapFun enumed                     (*O(|S|) work, O(1) span)*)
    val lengths = Seq.filter (fn i => not (i = ~1)) lengths (*O(|S|) work, O(log|S|) span*)

    fun tabFun i = (*O(1) tabulate function*)
      let
        val length =
          if i >= ((Seq.length lengths) - 1) then (Seq.length sorted) - (Seq.nth lengths (i)) (*O(1) work/span for length/nth*)
          else (Seq.nth lengths (i+1)) - (Seq.nth lengths i)
      in
        Seq.subseq sorted (Seq.nth lengths i, length)  (*O(1) work/span for subseq, nth*)
      end
  in
    Seq.tabulate tabFun (Seq.length lengths) (*O(n) work / O(1) span*)
  end

    (* Don't forget to comment the work and span of each line of your implementation so
     * you don't lose points! This means you shouldn't write a one-line
     * implementation. Consider using a let expression or helper functions
     * to break your code into chunks that are easy to reason about and comment.
     *)

  (* unique: ('a * 'a -> order) -> 'a Seq.t -> 'a Seq.t
   * REQUIRES:
   *    cmp is "well-behaved" (it is reflexive, anti-symmetric, and transitive)
   *    cmp is total and has O(1) work and span
   * ENSURES: unique cmp S ==> T where
   *    Each element of S has an EQUAL element in T
   *    The elements of T are in strictly increasing order (this prevents
   *      the existence of duplicates)
   *    unique cmp S has O(|S| log |S|) work and O(log^2 |S|) span
   *)

  fun unique cmp S = (*work/span is dominated by call to collected*)
  let
    val collected = collect cmp S                      (*O(|S|log|S|) work and O(log^2|S|) span*)
    val find = fn x => Seq.nth (Seq.nth collected x) 0 (*O(1) work/span for 2 nth*)
    val len = Seq.length collected                     (*O(1) work/span for length*)
  in
    Seq.tabulate find len                              (*O(|S|) work / O(1) Span for Tabulate*)
  end

end
