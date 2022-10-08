(* Imperative programming with lists *)
structure MutableList =
struct
  datatype 'a cell = Nil
                   | Cons of 'a * 'a llist
  withtype 'a llist = ('a cell) ref

  (* toList : 'a llist -> 'a list
   * REQUIRES: true
   * ENSURES: creates the corresponding 'a list from an 'a llist
   *)
  fun toList (l1 : 'a llist) : 'a list =
    case !l1 of
      Nil => []
    | Cons(x, r) => x:: toList r

  val example1 : int llist = ref Nil
  val example2 : int llist = ref (Cons (1, example1))
  val [1] = toList example2
  val () = example1 := (Cons (2, ref Nil))
  val [1,2] = toList example2

  (* map : ('a -> 'a) -> 'a llist -> unit
   * REQUIRES: f is total
   * ENSURES: mutates l so that each element x is replaced by f x
   *)
  fun map (f : 'a -> 'a) (l : 'a llist) : unit =
  case !l of
    Nil => ()
  | Cons(x, r) => (l := Cons(f x, r); map f r)

  val () = map (fn x => x * 4) example2
  val [4,8] = toList example2

  (* filter ('a -> bool) -> 'a llist -> unit
   * REQUIRES: p is total
   * ENSURES: mutates l to include each element x iff p x
   *)
  fun filter (p : 'a -> bool) (l : 'a llist) : unit =
    case !l of
      Nil => ()
    | Cons(x, r) => (l := (if p x then Cons(x, r) else !r); filter p r)

  val () = filter (fn x => x > 4) example2
  val [8] = toList example2

  (* append : 'a llist * 'a llist -> unit
   * REQUIRES: l1 is not ref Nil
   * ENSURES: appends l2 to l1
   *)
  fun append (l1 : 'a llist, l2 : 'a llist) : unit =
    case !l1 of
      Nil => ()
    | Cons(x, ref Nil) => (l1 := Cons(x,l2); ())
    | Cons(x, r) => append r l2


end
