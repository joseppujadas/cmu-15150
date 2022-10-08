type 'a Bool = 'a -> 'a -> 'a

val True  : 'a Bool = fn x => fn y => x
val False : 'a Bool = fn x => fn y => y

(* Filter: ('a' -> 'a list Bool) -> 'a list -> 'a list
 * REQUIRES: p total
 * ENSURES: Filter p L ==>* L' where L' is the list of all elements
                          x of L s.t. p x ==>* True, in the original
                          order of L
 *)
fun Filter (p : 'a -> 'a list Bool) ([] : 'a list)    : 'a list = []
  | Filter (p : 'a -> 'a list Bool) (x::xs : 'a list) : 'a list = ((p x) [x] [])@(Filter p xs)

(* Exists: ('a' -> 'b Bool Bool) -> 'a list -> 'b Bool
 * REQUIRES: p total
 * ENSURES: Exists p L ==>* True if there is some element x of L s.t.
                           p x ==>* True, and False if there is no such element
 *)
fun Exists (p : 'a -> 'a list Bool) ([] : 'a list) = False
  | Exists (p : 'a -> 'a list Bool) (x::xs : 'a list) = case p x [x] [] of
    [x] => True
  | [] => Exists p xs

datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree

(* Search : (’a -> ’a option Bool) -> ’a tree -> ’a option
 * REQUIRES: p total
 * ENSURES: Search p T ==>* SOME x if x is an element of T
                            s.t. p x ==>* True, and Search p T ==>* NONE
                             if there is no such element
 *)
fun Search p Empty = NONE
  | Search p Node(l,x,r) = 
