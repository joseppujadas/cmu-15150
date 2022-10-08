structure Frogs =
struct

fun toString (S : int Seq.seq) : string =
  Seq.mapreduce (Int.toString) ("") (fn (a,b) => a ^ "," ^ b) S


fun allJump (S : int Seq.seq) (a : int) : int Seq.seq =
  let
    fun tabFun i = Seq.nth S ((i-a) mod (Seq.length S))
  in
    Seq.tabulate tabFun (Seq.length S)
  end

fun possibleJumps (S1 : int Seq.seq) (S2 : int Seq.seq) : int Seq.seq =
  let
    val r = Seq.tabulate (fn i => i) (Seq.length S1)
    fun filterFun x = Seq.equal (op =) (allJump S1 x, S2)
  in
    Seq.filter filterFun r
  end

fun minJumps (S1 : int Seq.seq) (S2 : int Seq.seq) : (int * int) Seq.seq = raise Fail "Unimplemented"

fun minJumps2 (S1 : int Seq.seq) (S2 : int Seq.seq) : (int * int) Seq.seq = raise Fail "Unimplemented"


fun prefixSum (S : int Seq.seq) : int Seq.seq =
    Seq.tabulate (fn i => Seq.reduce (op +) 0 (Seq.take S (i+1))) (Seq.length S)


fun subseqSum (S : int Seq.seq) : (int * int) -> int =
  let
    val sums = prefixSum S
  in
    fn (a,b) => if (a = b) then Seq.nth S a else ((Seq.nth b) - (Seq.nth S a))
  end

end
