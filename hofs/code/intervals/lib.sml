(* split : 'a list -> ('a list * 'a list) *)
(* REQUIRES: true *)
(* ENSURES: split L => (A, B) s.t. L = A@B,
                |length A - length B| <= 1 *)
fun split ([] : 'a list) : ('a list * 'a list) = ([], [])
  | split ([x]) = ([x], [])
  | split (x :: y :: L) =
    let
      val (L', R') = split L
    in
      (x :: L', y :: R')
    end

(* merge : ('a * 'a -> order) -> ('a list * 'a list) -> 'a list *)
(* REQUIRES: f is total *)
(* ENSURES: merge f (A, B) => L s.t. L = A @ B,
                   L is sorted by the function f *)
fun merge (f : 'a * 'a -> order) ([] : 'a list, B : 'a list) = B
  | merge f (A, []) = A
  | merge f (x :: A, y :: B) = case f (x, y) of
    LESS => x :: (merge f (A, y :: B))
    | EQUAL => x :: y :: (merge f (A, B))
    | GREATER => y :: (merge f (x :: A, B))

(* msort : ('a * 'a -> order) -> 'a list -> 'a list *)
(* REQUIRES: f is a total function *)
(* ENSURES: msort f L => L', L' is a sorted permutation of L in O(n log n) *)
fun msort (f : 'a * 'a -> order) ([] : 'a list) : 'a list = []
  | msort f [x] = [x]
  | msort f L =
    let
      val (A, B) = split L
    in
      merge f (msort f A, msort f B)
    end
