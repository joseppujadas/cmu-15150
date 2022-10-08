(* defining type 'a ord *)
type 'a ord = 'a * 'a -> order

fun compareLists (cmp : 'a ord, L1 : 'a list, L2 : 'a list) : bool =
    case (L1, L2) of
        ([], []) => true
      | (_, []) => false
      | ([], _) => false
      | (x::xs, y::ys) => cmp (x, y) = EQUAL andalso compareLists (cmp, xs, ys)

(* Use this function to test your implementation of pivotSplit
 * with the corresponding comparison function *)
fun compareListsPairs (cmp, (L1, L2), (R1, R2)) =
    compareLists (cmp, L1, R1) andalso compareLists (cmp, L2, R2)

(* Use this function to test your implementation of quicksort
 * with the corresponding comparison function *)
fun isSorted (cmp : 'a ord, L : 'a list) : bool =
    case L of
         [] => true
    | x::[] => true
    | x::y::xs => cmp (x, y) <> GREATER andalso isSorted (cmp, y::xs)


(* pivotSplit : 'a ord * 'a * 'a list -> 'a list * 'a list
 * REQUIRES: cmp is an order function
 * ENSURES: pivotSplit (cmp,p,L) returns (A,B) such that A @ B is a
 *   permutation of L, where all elements in A are less than p, and all elements
 *   in B are greater than or equal to p. Also, for elements x, y : 'a and
 *   for L : 'a list, if x occurs earlier than y and both x and y are in the
 *   same partition, then x occurs earlier than y in the partition. *)
fun pivotSplit (cmp : 'a ord, p : 'a, [] : 'a list) : 'a list * 'a list = ([],[])
  | pivotSplit (cmp, p, x::xs) =
    let
      val (L,G) = pivotSplit (cmp, p, xs)
    in
      case cmp (x,p) of
        LESS => (x::L, G)
         | _ => (L, x::G)
    end

val L = [4,~2,6,4,~3,2,6,7]
val ([~2,~3],[4,6,4,2,6,7]) = pivotSplit (Int.compare, 0, L)
val ([~2,~3,2],[4,6,4,6,7]) = pivotSplit (Int.compare, 4, L)

val L' = ["qwerty","fun","doop","wow","cool"]
val (["doop","cool"],["qwerty","fun","wow"]) = pivotSplit (String.compare, "extensional", L')

(* quicksort : 'a ord * 'a list -> 'a list
 * REQUIRES: cmp is an order function
 * ENSURES: quicksort (cmp,L) evaluates to a cmp-sorted permutation of L
 *)
fun quicksort (cmp : 'a ord, [] : 'a list) : 'a list = []
  | quicksort (cmp, x::xs) =
    let
      val (A,B) = pivotSplit(cmp,x,xs)
    in
      let
        val (aSort,bSort) = (quicksort(cmp,A), quicksort(cmp,B))
      in
        aSort @ x :: bSort
      end
    end

val true = isSorted (Int.compare, quicksort (Int.compare, L))
val true = compareLists (Int.compare, quicksort(Int.compare, L), [~3,~2,2,4,4,6,6,7])
val true = isSorted (Int.compare, quicksort (Int.compare, []))
