structure Stream : STREAM =
struct
  datatype 'a stream = Stream of unit -> 'a front
  and 'a front = Empty | Cons of 'a * 'a stream

  exception EmptyStream


  val delay = Stream
  fun expose (Stream d) = d ()


  val empty = Stream (fn () => Empty)

  fun cons (x,s) = Stream (fn () => Cons (x,s))

  (* "Append" one stream to another.  Of course, if the first stream
      is infinite, we'll never actually get to the second stream. *)
  fun append (s1,s2) = delay (fn () => append' (expose s1, s2))
  and append' (Empty, s2) = expose s2
    | append' (Cons (x,s1), s2) = Cons (x, append (s1, s2))

  fun fromList L = (foldr append empty) (List.map (fn x => cons (x,empty)) L)

  fun tabulate f = delay (fn () => tabulate' f)
  and tabulate' f = Cons (f 0, tabulate (fn i => f (i+1)))

  (* functions null, hd, tl, map, filter, exists, take, drop *)
  (* parallel the functions in the List structure *)
  fun null s =
    case expose s of
      Empty => true
    | Cons _ => false

  fun hd s =
    case expose s of
      Empty => raise EmptyStream
    | Cons (x,_) => x

  (* take (s,n) converts the first n elements of n to a list *)
  (* raises Subscript if n < 0 or n >= length s *)
  fun takePos (s, 0) = nil
    | takePos (s, n) = take' (expose s, n)
  and take' (Empty, _) = raise Subscript
    | take' (Cons (x,s), n) = x :: takePos (s, n-1)

  fun take (s,n) = if n < 0 then raise Subscript else takePos (s,n)

  fun toList S =
    case expose S of
      Empty => []
    | Cons (x, S') => x::toList S'

  fun tl s =
    case expose s of
      Empty => raise EmptyStream
    | Cons (_,s) => s

  fun dropPos (s, 0) = s
    | dropPos (s, n) = drop' (expose s, n)
  and drop' (Empty, _) = raise Subscript
    | drop' (Cons (x,s), n) = dropPos (s, n-1)

  fun drop (s,n) = if n < 0 then raise Subscript else dropPos (s,n)

(* too eager---doesn't terminate on infinite streams
  fun map f s = map' f (expose s)
  and map' f Empty = empty
    | map' f (Cons (x,s)) = cons (f x, map f s)
*)

  fun map f s = delay (fn () => map' f (expose s))
  and map' f Empty = Empty
    | map' f (Cons (x,s)) = Cons (f x, map f s)

  fun filter p s = delay (fn () => filter' p (expose s))
  and filter' p Empty = Empty
    | filter' p (Cons (x,s)) =
        if p x then Cons (x, filter p s)
        else filter' p (expose s)

  fun zip (s1, s2) = delay (fn () => zip'(expose s1, expose s2))
  and zip' (_, Empty) = Empty
    | zip' (Empty, _) = Empty
    | zip' (Cons (x,s1), Cons (y,s2)) = Cons ((x,y), zip (s1,s2))


end
