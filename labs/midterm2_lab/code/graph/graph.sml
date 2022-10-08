type graph = (int * int list) list

(* neighbors : graph -> int -> int list
* REQUIRES: a is a vertex in the directed graph G
* ENSURES: (neighbors G a) returns a list
* containing the neighbors of the vertex a in G.
*)
fun neighbors ([] : graph) a = raise Fail "invalid"
  | neighbors ((b, xs) :: L) a =
    if a = b then xs else neighbors L a


(* replace : graph -> (int * int list) -> graph
 * REQUIRES: a is a vertex in the directed graph G,
 * and vs is a subset of G's vertices
 * ENSURES: replace G (a,vs) returns a new graph,
 * where the neighbors list of a is replaced with vs.
 *)
fun replace ([] : graph) _ = raise Fail "invalid"
  | replace ((b, xs) :: L) (a, vs) =
    if a = b then (b, vs) :: L
    else (b, xs) :: (replace L (a, vs))


(* pathCPS :
 *    graph
 * -> int
 * -> int
 * -> (int list -> 'a)
 * -> (unit -> 'a)
 * -> 'a
 * REQUIRES: a, b are vertices in the directed graph G
 * ENSURES: (pathCPS G a b sc fc) returns
 * sc [a,..,b] if there is a path [a,..,b], and
 * fc () otherwise.
 *)
fun pathCPS _ = raise Fail "Unimplemented"


(* vertices: graph -> int list
 * REQUIRES: true
 * ENSURES: (vertices G) returns a list of all vertices of G
 *)
fun vertices [] = []
  | vertices ((a, _) :: xs) = a :: (vertices xs)


(* cycleCPS :
 *    graph
 * -> (int list -> 'a)
 * -> (unit -> 'a)
 * -> 'a
 * REQUIRES: true
 * ENSURES: (cycleCPS G sc fc) returns
 * sc [a,..,a] if there is a path [a,..,a] for some vertex a in G, and
 * fc () otherwise.
 *)
fun cycleCPS _ = raise Fail "Unimplemented"


(* For testing purposes *)
val G1 = [
         (1, [2]),
         (2, [3]),
         (3, [4]),
         (4, [1,5]),
         (5, [6]),
         (6, [])
        ]

val G2 = [
         (1, [2]),
         (2, []),
         (3, [4]),
         (4, [1]),
         (5, [6]),
         (6, [1,3])
         ]

val success = SOME
val failure = fn () => NONE
