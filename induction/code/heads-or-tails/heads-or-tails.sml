(* heads : int * int list -> int
 * REQUIRES: true
 * ENSURES: heads (x, xs) ==>* the largest n such that the first n elements of xs
 * are all equal to x
 *)
fun heads (x : int, xs : int list) : int =
  case xs of
  [] => 0
  | y::ys =>
    if y = x then 1 + heads (x,ys)
    else 0

(* TEST CASES *)
(* Write test cases here *)
val 2 = heads (1,[1,1,2,1,3])
val 0 = heads (2,[1,1,2,1,3])
val 3 = heads (5,[5,5,5,0,5,5,5,5,5])
val 5 = heads (4, [4,4,4,4,4])


(* tails : int * int list -> int list
 * REQUIRES: true
 * ENSURES: tails (x, xs) ==>* xs with the first k elements removed, where
 * k = heads (x, xs)
 *)
fun tails (x : int, xs : int list) : int list =
  case xs of
  [] => []
  | y::ys =>
    if y = x then tails (x,ys)
    else y::ys

(* TEST CASES *)
(* Write test cases here *)
val [2,1,3] = tails (1,[1,1,2,1,3])
val [1,1,2,1,3] = tails (2,[1,1,2,1,3])
val [1] = tails (4, [4,4,4,4,4,4,1])
val [] = tails (2, [2,2])


(* remove : int * int list -> int list
 * REQUIRES: true
 * ENSURES: remove (x, xs) ==>* xs' such that xs' does not contain any element
 * y such that y = x, and xs' contains exactly all elements in xs except for x
 * in the same order as they were in xs
 *)
fun remove (x : int, xs : int list) : int list =
  case xs of
  [] => []
  | y::ys =>
  if y = x then remove(x,ys)
  else y::remove(x,ys)

(* TEST CASES *)
(* Write test cases here *)
val [] = remove (5,[5,5,5,5])
val [2,3] = remove (1,[1,1,2,1,3])
val [1,1,1,3] = remove (2,[1,1,2,1,3])
val [1,5,1,5,0] = remove (3,[1,5,1,5,0])


(* partition : int * int list -> int list * int list
 * REQUIRES: true
 * ENSURES: partition (pivot, L) ==>* (L1, L2) such that
 *          for all x in L1, x <= pivot and
 *          for all y in L2, pivot < y.
 *)
fun partition (pivot : int, L : int list) : int list * int list =
  case L of
  [] => ([],[])
  | x::xs =>
    let
      val (y1,y2) = partition(pivot,xs)
    in
      if x > pivot then (y1,x::y2)
      else (x::y1,y2)
    end

(* TEST CASES *)
(* Write test cases here *)
val ([1,0],[3,4,5]) = partition (2,[1,3,4,0,5])
val ([1,2,3,0],[]) = partition (3,[1,2,3,0])
val ([],[5,6,7,8]) = partition (0,[5,6,7,8])
val ([0,1],[39842,39843]) = partition(1,[0,1,39842,39843])
