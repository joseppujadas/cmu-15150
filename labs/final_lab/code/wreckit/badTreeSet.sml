signature SET =
sig
  type t
  type elem

  val emp : t
  val ins : t * elem -> t
  val lookup : t * elem -> bool

  val toList : t -> elem list
end

signature ORDERED =
sig
  type t

  val cmp : t * t -> order
end

datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree

(*INVARIANT: For a TreeSet S, if an element has been inserted into a
*            S, lookup S evaluates to true. S must be ordered according to O.cmp *)
functor TreeSet (O : ORDERED) : SET =
struct
  type t = O.t tree
  type elem = O.t

  val emp = Empty

  fun ins (Empty, x) = Node (Empty, x, Empty)
    | ins (Node (L, v, R), x) =
        case O.cmp (x, v) of
          LESS    => Node (ins (L, x), v, R)
        | GREATER => Node (L, v, ins (R, x))
        | EQUAL   => Node (L, x, R)

  fun lookup (Empty, x) = false
    | lookup (Node (L, v, R), x) =
      case O.cmp (x, v) of
          LESS    => lookup (L, x)
        | GREATER => lookup (R, x)
        | EQUAL   => true

  fun toList Empty = []
    | toList (Node (L, x, R)) = toList L @ (x :: toList R)
end
