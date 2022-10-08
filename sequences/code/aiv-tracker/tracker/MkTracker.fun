functor MkTracker (Utils: UTILS) : TRACKER =
struct

  (* you may assume this is a refsol for utils *)
  val unique = Utils.unique

  type mascot = int
  type chat = mascot Seq.t

  (*Tracker stores a sequence of int sequences, where the ith item
  is the ith mascot and holds all chats it is represented in, as
  well as the total number of chats *)
  type tracker = int Seq.t Seq.t

  (* empty: int -> tracker
   * REQUIRES:
   *    n >= 0
   * ENSURES:
   *    empty n reflects that there are no group chats created and a mascot
   *      index is from 0 to n-1. This representation should support all
   *      calls to addChat and query
   *)
  fun empty (n : int) : tracker = (Seq.tabulate (fn x => Seq.singleton x) n) (*O(n) work / O(1) span for tabolate, O(1) both for Empty*)

  (* addChat: tracker -> chat -> tracker
   * REQUIRES:
   *    Every member of the new chat has an index in 0 to n-1
   *    The chat has no duplicates
   *    T can be produced through calls to empty and addChat
   * ENSURES:
   *    addChat T C represents both C and the chats in T while maintaining the
   *      necessary invariants to give the right answer under future calls
   *      to addChat and query.
   *    O(|C| n log n) work and O(log^2 n) span.
   *)
  fun addChat (S : tracker) (C : chat) : tracker = (*Total work/span dominated by call to unique over cmap, making O(C n log n) work and O(log^2 n) span*)
    let
      fun mapFun m = (m, unique Int.compare (Seq.append (C, Seq.nth S m))) (*O(n) work O(1) span for append, O(1) both for nth, O(nlogn) work and O(log^2 n) span for unique*)

      val cmap = Seq.map mapFun C  (*O(C) work / O(1) span for map*)
    in
      Seq.inject (S, cmap) (*O(n + c) work / O(1) span for inject*)
    end



  (* query: tracker -> mascot -> int -> mascot Seq.t
   * REQUIRES:
   *    The mascot has an index from 0 to n-1
   *    The resolution is non-negative
   *    T can be produced through calls to empty and addChat
   * ENSURES: query T m r ==> A
   *    A contains every mascot from 0 to n − 1 such that there exists mascots
   *      s0, s1, . . . , sr where s0 = m and for all 0 ≤ i < r, si and
   *      s(i+1) share a chat or si = s(i+1)
   *    O(r n^2 log n) work and O(log r * log^2 n) span
   *)

    fun query (T : tracker) (m : mascot) (0 : int) : mascot Seq.t = Seq.singleton m
      | query T m 1 = Seq.nth T m
      | query T m r =
      let
        val rSeq = Seq. tabulate (fn _ => T) r

        fun reduceFun (_,y) =
          let
            val u = unique Int.compare (*O(n log n) work and O(log^2 n) span*)

            fun mapFun x =
              u (Seq.flatten (Seq.map (fn m => Seq.append (x, Seq.nth y m)) x)) (*O(n) work and O(1) span*)
          in
            Seq.map mapFun y (*O(n) work and O(1) span, O(n^3 log n) work and O(log^2 n) span given the function*)
          end

        val reduced = Seq.reduce1 reduceFun rSeq (*O(r) work and O(log r) span for reduce1, O(r n^3 log n) work and O(log r log^2 n) span total*)

      in
        Seq.nth reduced m (*O(1) work / span*)
      end


end
