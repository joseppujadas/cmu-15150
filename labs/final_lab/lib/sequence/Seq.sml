(**
 * 15-150 Sequence Implementation
 *
 * Credit to the 15-210 Sequence
 * implementation from which this is modified
 * in order to ensure standardization between classes.
 *
 * Implements SEQUENCE with
 *   type 'a seq = 'a ArraySlice.slice
 *)

structure Seq :> SEQUENCE =
struct

  structure A = Array
  structure AS = ArraySlice

  type 'a t = 'a AS.slice
  type 'a seq = 'a t
  type 'a ord = 'a * 'a -> order

  datatype 'a lview = Nil | Cons of 'a * 'a seq
  datatype 'a tview = Empty | Leaf of 'a | Node of 'a seq * 'a seq

  exception Range of string

  (* This would be here for parallelizing if we could do that *)
  fun par (x, y) = (x (), y ())

  val length : 'a seq -> int = AS.length

  fun null s = length s = 0

  fun empty _ = AS.full (A.fromList [])

  fun singleton x = AS.full (A.fromList [x])

  val $ = singleton

  fun tabulate f n =
    if n < 0 then raise Range "tabulate index out of bounds"
    else AS.full (A.tabulate (n, f)) handle Size => raise Range "tabulate index out of bounds"

  fun nth s i =
    AS.sub (s, i) handle Subscript => raise Range "nth index out of bounds"

  fun toString f s =
    "<" ^ String.concatWith ", " (List.tabulate (length s, f o nth s)) ^ ">"

  fun fromList l = AS.full (A.fromList l)

  val % = fromList

  fun subseq s (i, len) =
    if len < 0
    then raise Range "subseq index out of bounds"
    else AS.subslice (s, i, SOME len) handle Subscript => raise Range "subseq index out of bounds"

  fun take s n = subseq s (0, n)
  fun drop s n = subseq s (n, length s - n)

  fun append (s, t) =
    let val (ns, nt) = (length s, length t)
        fun ith i = if i >= ns then nth t (i-ns) else nth s i
    in tabulate ith (ns+nt)
    end

  fun showl s =
    case length s
     of 0 => Nil
      | l => Cons(nth s 0, drop s 1)

  fun hidel Nil = empty ()
    | hidel (Cons (x,xs)) = append(singleton x, xs)

  fun showt s =
    case length s
     of 0 => Empty
      | 1 => Leaf (nth s 0)
      | n => Node (take s (n div 2), drop s (n div 2))

  fun hidet Empty = empty()
    | hidet (Leaf x) = singleton x
    | hidet (Node(s1,s2)) = append (s1, s2)

  fun cons x s = hidel(Cons(x, s))

  fun split s i = (take s i, drop s i)

  fun rev s =
    tabulate (fn i => nth s (length s - 1 - i)) (length s)

  fun iteratePrefixes f b s =
    let
  fun iter s (old, cur) =
    case showl s of
        Nil => (rev (fromList old), cur)
     |  Cons (x, xs) => iter xs (cur::old, f (cur, x))
    in iter s ([], b)
    end

  fun iteratePrefixesIncl f b s =
    let val (prefixes, final) = iteratePrefixes f b s
    in drop (append (prefixes, singleton final)) 1
    end

  fun iterate f b s = #2 (iteratePrefixes f b s)

  fun toList s = iterate (fn (l,x) => x::l) [] (rev s)

  fun repeat n x = tabulate (fn _ => x) n

  fun enum s = tabulate (fn i => (i, nth s i)) (length s)
  fun map f s = tabulate (f o (nth s)) (length s)
  fun mapIdx f s = tabulate (fn i => f (i, nth s i)) (length s)

  fun zipWith f (s, t) =
    tabulate (fn i => f (nth s i, nth t i)) (Int.min (length s, length t))

  fun unzipWith (spl : 'a -> 'b * 'c) s =
    let val s' = map spl s
    in (tabulate (#1 o nth s') (length s), tabulate (#2 o nth s') (length s))
    end

  fun zip (s, t) = zipWith (fn x => x) (s, t)
  fun unzip s = unzipWith (fn x => x) s

  fun reduce1 f s =
    case showt s of
        Leaf x => x
      | Node (l, r) => f (reduce1 f l, reduce1 f r)
      | Empty => raise Range "reduce1 called on empty sequence"

  fun reduce f b s = reduce1 f (append (s, singleton b))

  fun mapreduce f z g s = reduce g z (map f s)

  fun merge cmp (s, t) =
    let
  (* Sequential merge. Pretend it's parallel! *)
  fun merge' ([], t) = t
    | merge' (s, []) = s
    | merge' (x::xs, y::ys) =
      if cmp (y, x) = LESS
      then y :: merge' (x::xs, ys)
      else x :: merge' (xs, y::ys)
    in
  fromList (merge' (toList s, toList t))
    end

  fun sort cmp s = reduce (merge cmp) (empty ()) (map singleton s)


  (* note: assuming b is an identity for f *)
  fun scan f b s =
    case length s of
  0 => (empty (), b)
     |  1 => (singleton b, nth s 0)
     |  n =>
  let
      fun contract i =
        if i = n div 2 then nth s (2*i)
        else f (nth s (2*i), nth s (2*i+1))
      val (r, res) = scan f b (tabulate contract ((n+1) div 2))
      fun expand i =
        if i mod 2 = 0 then nth r (i div 2)
        else f (nth r (i div 2), nth s (i-1))
  in (tabulate expand n, res)
  end

  fun flatten ss =
    let
      val (starts, n) = scan op+ 0 (map length ss)
      val res = tabulate (fn _ => NONE) n
      fun write i (j,x) = AS.update (res, i+j, SOME x)
      val _ = map (fn (i,s) => mapIdx (write i) s) (zip (starts, ss))
    in map valOf res
    end

  fun filter p s =
    if length s = 0 then s else
    let
      val opts = map (fn e => if p e then SOME e else NONE) s
      val incrs = map (fn SOME _ => 1 | NONE => 0) opts
      val (psums, sz) = scan (op+) 0 incrs

      val res = AS.full (A.array (length s, nth s 0))
      fun update (SOME e, i) = AS.update (res, i, e)
        | update (NONE, _) = ()
    in
      AS.app update (zip (opts, psums));
      AS.subslice (res, 0, SOME sz)
    end

  fun filterIdx p =
    map (fn (_, x) => x) o (filter p) o enum

  (* pretend it's parallel! *)
  fun inject (s, updates) =
    let
      val res = tabulate (nth s) (length s)
      fun update (i,x) = AS.update (res, i, x) handle Subscript => raise Range "inject index out of bounds"
    in
      AS.app update updates;
      res
    end

  fun update (s, (i, x)) = inject (s, singleton (i, x))

  exception NotFound

  fun int_binsearch test low high =
    if (low = high) andalso (test low <> EQUAL) then raise NotFound
    else
        let
            val midpoint = (low + high) div 2
        in
            case test midpoint of
                EQUAL => midpoint
              | LESS =>
    if midpoint = low then
                    (* Without this case we'd infloop at the end of a search *)
                    int_binsearch test high high
    else
                    int_binsearch test midpoint high
              | GREATER => int_binsearch test low midpoint
        end

  fun find_first test low high =
    if (low = high) then low
    else let
      val midpoint = (low + high) div 2
    in
      case test midpoint of
           EQUAL => find_first test low midpoint
         | _ =>
             if midpoint = low then
               find_first test high high
             else
               find_first test midpoint high
    end

  fun search cmp x s =
    if length s = 0 then
      NONE
    else
      (let
        fun test i = cmp(nth s i, x)
        val result = int_binsearch test 0 ((length s - 1))
      in
        SOME (find_first test 0 result)
      end) handle NotFound => NONE

  fun equal cmp (s1,s2) =
    length s1 = length s2 andalso
    reduce (fn (x,y) => x andalso y) true (zipWith cmp (s1, s2))

  fun argmax cmp s =
    let
  fun best (a, b) =
          case (a, b) of
              (NONE, _) => b
            | (_, NONE) => a
            | (SOME (i, x), SOME (j, y)) =>
              if cmp (y, x) = GREATER then SOME (j, y) else SOME (i, x)
    in
  case reduce best NONE (mapIdx SOME s) of
            NONE => raise Range "sequence passed to argmax must be non-empty"
    | SOME (i, _) => i
    end

end
