signature TREES =
sig
  datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree

  val depth: 'a tree -> int
  val weightSum: int tree -> int
  val weightSumCPS: int tree -> (int -> 'a) -> 'a
  val genWeightSum: (int * int -> int) -> int tree -> int
end
