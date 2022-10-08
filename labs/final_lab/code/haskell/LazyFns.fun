functor LazyFns (Lazy : LAZY) : LAZYFNS =
struct
  structure Lazy = Lazy

  (* You need to repeat the following infix declaration in the REPL when using
   * these functions because SML doesn't export fixity declarations from modules
   *)
  infix && ||

  fun x && y = Lazy.force x andalso Lazy.force y

  fun pure x = Lazy.delay (fn () => x)

  fun map f x = Lazy.delay (fn () => f (Lazy.force x))

  fun join x = Lazy.delay (fn () => Lazy.force (Lazy.force x))

  fun mkStream L = Stream.delay (fn () =>
    case Lazy.force L of
      [] => Stream.Empty
    | x::xs => Stream.Cons(x,mkStream (Lazy.delay (fn () => xs))))
end
