local
(* NOTE TO INSTRUCTORS WORKING ON THIS HOMEWORK: *)
(* THIS IS AN AUTOMATICALLY GENERATED FILE. *)
(* CHANGES YOU MAKE IN HERE WILL PROBABLY GET OVERWRITTEN. *)

signature SUSP =
   sig

      type 'a susp

      val delay : (unit -> 'a) -> 'a susp
      val force : 'a susp -> 'a

   end
structure Susp :> SUSP = SMLofNJ.Susp


signature STREAM =
   sig

      type 'a stream
      datatype 'a front = Nil | Cons of 'a * 'a stream

      val front : 'a stream -> 'a front
      val eager : 'a front -> 'a stream
      val lazy : (unit -> 'a front) -> 'a stream

      val fromProcess : (unit -> 'a option) -> 'a stream
      val fromList : 'a list -> 'a stream
      val fromString : string -> char stream
      val fromTextInstream : TextIO.instream -> char stream
      val fromBinInstream : BinIO.instream -> Word8.word stream
      val fromLoop : ('a -> ('a * 'b) option) -> 'a -> 'b stream

      val fix : ('a stream -> 'a stream) -> 'a stream

      exception Empty
      val hd : 'a stream -> 'a
      val tl : 'a stream -> 'a stream
      val take : 'a stream * int -> 'a list
      val drop : 'a stream * int -> 'a stream
      val map : ('a -> 'b) -> 'a stream -> 'b stream

   end

structure Stream
   :> STREAM
   =
   struct

      open Susp

      datatype 'a front = Nil | Cons of 'a * 'a stream
      withtype 'a stream = 'a front susp

      val front = force
      fun eager f = delay (fn () => f)
      val lazy = delay

      fun fromProcess f =
          lazy
          (fn () =>
                 (case f () of
                     NONE =>
                        Nil
                   | SOME x =>
                        Cons (x, fromProcess f)))

      fun fromList l =
          lazy
          (fn () =>
                 (case l of
                     [] => Nil
                   | h :: t => Cons (h, fromList t)))

      fun fromString str =
          let
             val n = size str

             fun loop i =
                 lazy
                 (fn () =>
                        if i >= n then
                           Nil
                        else
                           Cons (String.sub (str, i), loop (i+1)))
          in
             loop 0
          end

      fun fromTextInstream ins =
          fromProcess (fn () => TextIO.input1 ins)

      fun fromBinInstream ins =
          fromProcess (fn () => BinIO.input1 ins)

      fun fromLoop f seed =
          lazy
          (fn () =>
              (case f seed of
                  NONE =>
                     Nil
                | SOME (seed', x) =>
                     Cons (x, fromLoop f seed')))


      fun fix f = f (lazy (fn () => front (fix f)))

      exception Empty

      fun hd s =
          (case front s of
              Nil =>
                 raise Empty
            | Cons (x, _) =>
                 x)

      fun tl s =
          (case front s of
              Nil =>
                 raise Empty
            | Cons (_, s') =>
                 s')

      fun take (s, n) =
          if n < 0 then
             raise Subscript
          else if n = 0 then
             []
          else
             (case front s of
                 Nil =>
                    raise Subscript
               | Cons (x, s') =>
                    x :: take (s', n-1))

      fun drop (s, n) =
          if n < 0 then
             raise Subscript
          else if n = 0 then
             s
          else
             (case front s of
                 Nil =>
                    raise Subscript
               | Cons (x, s') =>
                    drop (s', n-1))

      fun map f s =
          lazy
          (fn () =>
                 (case front s of
                     Nil =>
                        Nil
                   | Cons (x, s') =>
                        Cons (f x, map f s')))
   end

signature STREAMABLE =
   sig
      type 'a t

      datatype 'a front = Nil | Cons of 'a * 'a t
      val front : 'a t -> 'a front
   end

structure ListStreamable
   :> STREAMABLE
      where type 'a t = 'a list
   =
   struct

      type 'a t = 'a list
      datatype 'a front = Nil | Cons of 'a * 'a list

      fun front l =
         (case l of
             [] => Nil
           | h :: t => Cons (h, t))

   end


structure StreamStreamable
   :> STREAMABLE
      where type 'a t = 'a Stream.stream
   =
   struct

      type 'a t = 'a Stream.stream
      datatype front = datatype Stream.front
      val front = Stream.front

   end


functor CoercedStreamable (structure Streamable : STREAMABLE
                           type 'a item
                           val coerce : 'a item -> 'a)
   :> STREAMABLE
      where type 'a t = 'a item Streamable.t
   =
   struct

      type 'a t = 'a item Streamable.t

      datatype 'a front = Nil | Cons of 'a * 'a item Streamable.t

      fun front s =
         (case Streamable.front s of
             Streamable.Nil => Nil
           | Streamable.Cons (x, s') =>
                Cons (coerce x, s'))

   end

signature LEX_ENGINE =
   sig

      structure Streamable : STREAMABLE

      type symbol

      val next7x1 : int -> string -> int -> int -> int
      val next8x1 : int -> string -> int -> int -> int
      val next9x1 : int -> string -> int -> int -> int
      val next10x1 : int -> string -> int -> int -> int
      val next7x2 : int -> string -> int -> int -> int
      val next8x2 : int -> string -> int -> int -> int
      val next9x2 : int -> string -> int -> int -> int
      val next10x2 : int -> string -> int -> int -> int

      val next0x1 : string -> int -> int
      val next0x2 : string -> int -> int

      type ('a, 'b) action =
         { match : symbol list,
           len : int,
           start : symbol Streamable.t,
           follow : symbol Streamable.t,
           self : 'b } -> 'a

      type ('a, 'b) table =
         int * int * int * ('a, 'b) action vector * (int -> int -> int) * (int -> int)

      val lex : 'b -> ('a, 'b) table -> symbol Streamable.t -> 'a

   end

functor LexEngineFun (structure Streamable : STREAMABLE
                      type symbol
                      val ord : symbol -> int)
   :> LEX_ENGINE
      where type 'a Streamable.t = 'a Streamable.t
      and type symbol = symbol
   =
   struct

      structure Streamable = Streamable
      type symbol = symbol

      (* Next state function for 7-bit symbols, with 1-byte results. *)
      fun next7x1 symbolLimit table state symbol =
          if symbol >= symbolLimit then
             0
          else
             let
                val i = 128 * state + symbol
             in
                Char.ord (String.sub (table, i))
             end

      fun next8x1 symbolLimit table state symbol =
          if symbol >= symbolLimit then
             0
          else
             let
                val i = 256 * state + symbol
             in
                Char.ord (String.sub (table, i))
             end

      fun next9x1 symbolLimit table state symbol =
          if symbol >= symbolLimit then
             0
          else
             let
                val i = 512 * state + symbol
             in
                Char.ord (String.sub (table, i))
             end

      fun next10x1 symbolLimit table state symbol =
          if symbol >= symbolLimit then
             0
          else
             let
                val i = 1024 * state + symbol
             in
                Char.ord (String.sub (table, i))
             end

      fun next7x2 symbolLimit table state symbol =
          if symbol >= symbolLimit then
             0
          else
             let
                val i = 256 * state + 2 * symbol
             in
                Char.ord (String.sub (table, i)) * 256 +
                Char.ord (String.sub (table, i+1))
             end

      fun next8x2 symbolLimit table state symbol =
          if symbol >= symbolLimit then
             0
          else
             let
                val i = 512 * state + 2 * symbol
             in
                Char.ord (String.sub (table, i)) * 256 +
                Char.ord (String.sub (table, i+1))
             end

      fun next9x2 symbolLimit table state symbol =
          if symbol >= symbolLimit then
             0
          else
             let
                val i = 1024 * state + 2 * symbol
             in
                Char.ord (String.sub (table, i)) * 256 +
                Char.ord (String.sub (table, i+1))
             end

      fun next10x2 symbolLimit table state symbol =
          if symbol >= symbolLimit then
             0
          else
             let
                val i = 2048 * state + 2 * symbol
             in
                Char.ord (String.sub (table, i)) * 256 +
                Char.ord (String.sub (table, i+1))
             end


      (* Next state function for end-of-stream, with 1-byte results. *)
      fun next0x1 table state =
          Char.ord (String.sub (table, state))

      fun next0x2 table state =
          let
             val i = 2 * state
          in
             Char.ord (String.sub (table, i)) * 256 +
             Char.ord (String.sub (table, i+1))
          end


      type ('a, 'b) action = { match : symbol list,
                               len : int,
                               start : symbol Streamable.t,
                               follow : symbol Streamable.t,
                               self : 'b } -> 'a

      type ('a, 'b) table =
         int * int * int * ('a, 'b) action vector * (int -> int -> int) * (int -> int)

      fun lex self (initial, lastAcceptSink, lastAccept, acceptTable, next, nextEos) s =
          let
             fun loop candidate candLen candChars candStream state len chars s =
                if state = 0 then
                   (candidate, candLen, candChars, candStream)
                else if state <= lastAccept then
                   if state <= lastAcceptSink then
                      (state, len, chars, s)
                   else
                      (case Streamable.front s of
                          Streamable.Nil =>
                             loop state len chars s (nextEos state) len chars s
                        | Streamable.Cons (ch, s') =>
                             loop state len chars s (next state (ord ch)) (len+1) (ch :: chars) s')
                else
                   (case Streamable.front s of
                       Streamable.Nil =>
                          loop candidate candLen candChars candStream (nextEos state) len chars s
                     | Streamable.Cons (ch, s') =>
                          loop candidate candLen candChars candStream (next state (ord ch)) (len+1) (ch :: chars) s')

             val (acceptingState, len, chars, s') =
                (* By construction, initial is an accepting state. *)
                loop initial 0 [] s initial 0 [] s

             val f = Vector.sub (acceptTable, acceptingState-1)
          in
             f { match = rev chars,
                 len = len,
                 start = s,
                 follow = s',
                 self = self }
          end

   end

signature PARSE_ENGINE =
   sig

      structure Streamable : STREAMABLE

      type terminal
      type value

      val next5x1 : string -> int -> int -> int
      val next6x1 : string -> int -> int -> int
      val next7x1 : string -> int -> int -> int
      val next8x1 : string -> int -> int -> int
      val next9x1 : string -> int -> int -> int
      val next5x2 : string -> int -> int -> int
      val next6x2 : string -> int -> int -> int
      val next7x2 : string -> int -> int -> int
      val next8x2 : string -> int -> int -> int
      val next9x2 : string -> int -> int -> int

      type action = value list -> value list

      type 'a table =
         (int -> int -> int)             (* action table *)
         *
         (int -> int -> int)             (* goto table *)
         *
         (int * int * action) vector     (* reduction information: lhs ordinal, rhs size, function *)
         *
         (value -> 'a)                   (* result destructor *)
         *
         (terminal Streamable.t -> exn)  (* error function *)

      val parse : 'a table -> terminal Streamable.t -> 'a * terminal Streamable.t

   end

functor ParseEngineFun (structure Streamable : STREAMABLE
                        type terminal
                        type value
                        val dummy : value
                        val read : terminal -> int * value)
   :> PARSE_ENGINE where type 'a Streamable.t = 'a Streamable.t
                     and type terminal = terminal
                     and type value = value
   =
   struct

      structure Streamable = Streamable

      type terminal = terminal
      type value = value

      (* Transition function for a 5-bit symbols, with 1-byte results. *)
      fun next5x1 table state symbol =
         let
            val i = 32 * state + symbol
         in
            Char.ord (String.sub (table, i)) - 128
         end

      fun next6x1 table state symbol =
         let
            val i = 64 * state + symbol
         in
            Char.ord (String.sub (table, i)) - 128
         end

      fun next7x1 table state symbol =
         let
            val i = 128 * state + symbol
         in
            Char.ord (String.sub (table, i)) - 128
         end

      fun next8x1 table state symbol =
         let
            val i = 256 * state + symbol
         in
            Char.ord (String.sub (table, i)) - 128
         end

      fun next9x1 table state symbol =
         let
            val i = 512 * state + symbol
         in
            Char.ord (String.sub (table, i)) - 128
         end

      fun next5x2 table state symbol =
         let
            val i = 64 * state + 2 * symbol
         in
            Char.ord (String.sub (table, i)) * 256 +
            Char.ord (String.sub (table, i+1))
            - 32768
         end

      fun next6x2 table state symbol =
         let
            val i = 128 * state + 2 * symbol
         in
            Char.ord (String.sub (table, i)) * 256 +
            Char.ord (String.sub (table, i+1))
            - 32768
         end

      fun next7x2 table state symbol =
         let
            val i = 256 * state + 2 * symbol
         in
            Char.ord (String.sub (table, i)) * 256 +
            Char.ord (String.sub (table, i+1))
            - 32768
         end

      fun next8x2 table state symbol =
         let
            val i = 512 * state + 2 * symbol
         in
            Char.ord (String.sub (table, i)) * 256 +
            Char.ord (String.sub (table, i+1))
            - 32768
         end

      fun next9x2 table state symbol =
         let
            val i = 1024 * state + 2 * symbol
         in
            Char.ord (String.sub (table, i)) * 256 +
            Char.ord (String.sub (table, i+1))
            - 32768
         end


      type action = value list -> value list

      type 'a table =
         (int -> int -> int)             (* action table *)
         *
         (int -> int -> int)             (* goto table *)
         *
         (int * int * action) vector     (* reduction information: lhs, size of rhs, functions to call *)
         *
         (value -> 'a)                   (* result destructor *)
         *
         (terminal Streamable.t -> exn)  (* error function *)

      fun parse (action, goto, reduce, destruct, error) s =
         let
            fun loop ststack valstack s =
               (case Streamable.front s of
                   Streamable.Nil =>
                      loopRead ststack valstack 0 dummy s s
                 | Streamable.Cons (term, s') =>
                      let
                         val (ordinal, value) = read term
                      in
                         loopRead ststack valstack ordinal value s s'
                      end)

            and loopRead ststack valstack ordinal value s s' =
               (case ststack of
                   [] =>
                      raise (Fail "bad parsing table")
                 | state :: rest =>
                      let
                         val n = action state ordinal
                      in
                         if n = 0 then
                            raise (error s)
                         else if n > 0 then
                            (* shift *)
                            loop (n-1 :: ststack) (value :: valstack) s'
                         else if n = ~1 then
                            (* accept *)
                            (valstack, s)
                         else
                            (* reduce *)
                            let
                               val (lhs, rhssize, f) = Vector.sub (reduce, ~n - 2)
                               val ststack' = List.drop (ststack, rhssize)
                            in
                               loopRead (goto (List.hd ststack') lhs :: ststack') (f valstack) ordinal value s s'
                            end
                      end)

         in
            (case loop [0] [] s of
                ([value], s') => (destruct value, s')
              | _ =>
                   raise (Fail "bad parsing table"))
         end
   end
(* File generated by CM-Lex version 2.1 *)

functor CNotLexFun
   (structure Streamable : STREAMABLE
    structure Arg :
       sig
          type symbol
          val ord : symbol -> int

          type t

          type self = { commentblock : symbol Streamable.t -> t,
                        commentline : symbol Streamable.t -> t,
                        main : symbol Streamable.t -> t }
          type info = { match : symbol list,
                        len : int,
                        start : symbol Streamable.t,
                        follow : symbol Streamable.t,
                        self : self }

          val add : info -> t
          val assign : info -> t
          val break : info -> t
          val commentblock_newline : info -> t
          val continue : info -> t
          val entercommentblock : info -> t
          val entercommentline : info -> t
          val eof : info -> t
          val equal : info -> t
          val error : info -> t
          val greater : info -> t
          val greaterequal : info -> t
          val ident : info -> t
          val kwelse : info -> t
          val kwfalse : info -> t
          val kwif : info -> t
          val kwtrue : info -> t
          val kwwhile : info -> t
          val land : info -> t
          val lbrace : info -> t
          val less : info -> t
          val lessequal : info -> t
          val lnot : info -> t
          val loop : info -> t
          val lor : info -> t
          val lparen : info -> t
          val mult : info -> t
          val notequal : info -> t
          val number : info -> t
          val rbrace : info -> t
          val return : info -> t
          val rparen : info -> t
          val semi : info -> t
          val skip : info -> t
          val skip_newline : info -> t
          val sub : info -> t
       end)
   :>
   sig
      val commentblock : Arg.symbol Streamable.t -> Arg.t
      val commentline : Arg.symbol Streamable.t -> Arg.t
      val main : Arg.symbol Streamable.t -> Arg.t
   end
=

(*

AUTOMATON LISTINGS
==================

Automaton commentblock
initial state = 7
total states = 9

-----

commentblock state 6 (final:commentblock_newline):

10 => state 3   (sink:commentblock_newline)

-----

commentblock state 7 (initial, final:error):

0-9 => state 2   (sink:entercommentblock)
10 => state 1   (sink:commentblock_newline)
11-12 => state 2   (sink:entercommentblock)
13 => state 6   (final:commentblock_newline)
14-41 => state 2   (sink:entercommentblock)
42 => state 8   (final:entercommentblock)
43-127 => state 2   (sink:entercommentblock)
EOS => state 5   (sink:eof)

-----

commentblock state 8 (final:entercommentblock):

47 => state 4   (sink:skip)

=====

Automaton commentline
initial state = 6
total states = 7

-----

commentline state 5 (final:skip_newline):

10 => state 3   (sink:skip_newline)

-----

commentline state 6 (initial, final:error):

0-9 => state 2   (sink:entercommentline)
10 => state 1   (sink:skip_newline)
11-12 => state 2   (sink:entercommentline)
13 => state 5   (final:skip_newline)
14-127 => state 2   (sink:entercommentline)
EOS => state 4   (sink:eof)

=====

Automaton main
initial state = 21
total states = 77

-----

main state 20 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-113 => state 48   (final:ident)
114 => state 25   (final:ident)
115-122 => state 48   (final:ident)

-----

main state 21 (initial, final:error):

0 => state 36   (final:skip)
9 => state 36   (final:skip)
10 => state 11   (sink:skip_newline)
12 => state 36   (final:skip)
13 => state 43   (final:skip_newline)
32 => state 36   (final:skip)
33 => state 49   (final:lnot)
38 => state 72
40 => state 13   (sink:lparen)
41 => state 8   (sink:rparen)
42 => state 16   (sink:mult)
43 => state 6   (sink:add)
45 => state 3   (sink:sub)
47 => state 71
48-57 => state 70   (final:number)
58 => state 73
59 => state 9   (sink:semi)
60 => state 62   (final:less)
61 => state 74
62 => state 39   (final:greater)
65-90 => state 48   (final:ident)
97 => state 48   (final:ident)
98 => state 67   (final:ident)
99 => state 60   (final:ident)
100 => state 48   (final:ident)
101 => state 44   (final:ident)
102 => state 61   (final:ident)
103-104 => state 48   (final:ident)
105 => state 23   (final:ident)
106-107 => state 48   (final:ident)
108 => state 35   (final:ident)
109-113 => state 48   (final:ident)
114 => state 51   (final:ident)
115 => state 48   (final:ident)
116 => state 20   (final:ident)
117-118 => state 48   (final:ident)
119 => state 33   (final:ident)
120-122 => state 48   (final:ident)
123 => state 1   (sink:lbrace)
124 => state 75
125 => state 17   (sink:rbrace)
126 => state 76
EOS => state 19   (sink:eof)

-----

main state 22 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-114 => state 48   (final:ident)
115 => state 40   (final:ident)
116-122 => state 48   (final:ident)

-----

main state 23 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-101 => state 48   (final:ident)
102 => state 58   (final:kwif)
103-122 => state 48   (final:ident)

-----

main state 24 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-100 => state 48   (final:ident)
101 => state 65   (final:kwfalse)
102-122 => state 48   (final:ident)

-----

main state 25 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-116 => state 48   (final:ident)
117 => state 34   (final:ident)
118-122 => state 48   (final:ident)

-----

main state 26 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-116 => state 48   (final:ident)
117 => state 45   (final:ident)
118-122 => state 48   (final:ident)

-----

main state 27 (final:kwwhile):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-122 => state 48   (final:ident)

-----

main state 28 (final:kwtrue):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-122 => state 48   (final:ident)

-----

main state 29 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-104 => state 48   (final:ident)
105 => state 41   (final:ident)
106-122 => state 48   (final:ident)

-----

main state 30 (final:kwelse):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-122 => state 48   (final:ident)

-----

main state 31 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-109 => state 48   (final:ident)
110 => state 64   (final:ident)
111-122 => state 48   (final:ident)

-----

main state 32 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97 => state 47   (final:ident)
98-122 => state 48   (final:ident)

-----

main state 33 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-103 => state 48   (final:ident)
104 => state 29   (final:ident)
105-122 => state 48   (final:ident)

-----

main state 34 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-100 => state 48   (final:ident)
101 => state 28   (final:kwtrue)
102-122 => state 48   (final:ident)

-----

main state 35 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-110 => state 48   (final:ident)
111 => state 66   (final:ident)
112-122 => state 48   (final:ident)

-----

main state 36 (final:skip):

0 => state 36   (final:skip)
9 => state 36   (final:skip)
12 => state 36   (final:skip)
32 => state 36   (final:skip)

-----

main state 37 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-116 => state 48   (final:ident)
117 => state 69   (final:ident)
118-122 => state 48   (final:ident)

-----

main state 38 (final:continue):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-122 => state 48   (final:ident)

-----

main state 39 (final:greater):

61 => state 2   (sink:greaterequal)

-----

main state 40 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-100 => state 48   (final:ident)
101 => state 30   (final:kwelse)
102-122 => state 48   (final:ident)

-----

main state 41 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-107 => state 48   (final:ident)
108 => state 50   (final:ident)
109-122 => state 48   (final:ident)

-----

main state 42 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-109 => state 48   (final:ident)
110 => state 56   (final:return)
111-122 => state 48   (final:ident)

-----

main state 43 (final:skip_newline):

10 => state 11   (sink:skip_newline)

-----

main state 44 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-107 => state 48   (final:ident)
108 => state 22   (final:ident)
109-122 => state 48   (final:ident)

-----

main state 45 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-100 => state 48   (final:ident)
101 => state 38   (final:continue)
102-122 => state 48   (final:ident)

-----

main state 46 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-109 => state 48   (final:ident)
110 => state 26   (final:ident)
111-122 => state 48   (final:ident)

-----

main state 47 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-106 => state 48   (final:ident)
107 => state 63   (final:break)
108-122 => state 48   (final:ident)

-----

main state 48 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-122 => state 48   (final:ident)

-----

main state 49 (final:lnot):

61 => state 18   (sink:notequal)

-----

main state 50 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-100 => state 48   (final:ident)
101 => state 27   (final:kwwhile)
102-122 => state 48   (final:ident)

-----

main state 51 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-100 => state 48   (final:ident)
101 => state 52   (final:ident)
102-122 => state 48   (final:ident)

-----

main state 52 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-115 => state 48   (final:ident)
116 => state 37   (final:ident)
117-122 => state 48   (final:ident)

-----

main state 53 (final:loop):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-122 => state 48   (final:ident)

-----

main state 54 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-111 => state 48   (final:ident)
112 => state 53   (final:loop)
113-122 => state 48   (final:ident)

-----

main state 55 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-107 => state 48   (final:ident)
108 => state 68   (final:ident)
109-122 => state 48   (final:ident)

-----

main state 56 (final:return):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-122 => state 48   (final:ident)

-----

main state 57 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-100 => state 48   (final:ident)
101 => state 32   (final:ident)
102-122 => state 48   (final:ident)

-----

main state 58 (final:kwif):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-122 => state 48   (final:ident)

-----

main state 59 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-104 => state 48   (final:ident)
105 => state 46   (final:ident)
106-122 => state 48   (final:ident)

-----

main state 60 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-110 => state 48   (final:ident)
111 => state 31   (final:ident)
112-122 => state 48   (final:ident)

-----

main state 61 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97 => state 55   (final:ident)
98-122 => state 48   (final:ident)

-----

main state 62 (final:less):

61 => state 10   (sink:lessequal)

-----

main state 63 (final:break):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-122 => state 48   (final:ident)

-----

main state 64 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-115 => state 48   (final:ident)
116 => state 59   (final:ident)
117-122 => state 48   (final:ident)

-----

main state 65 (final:kwfalse):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-122 => state 48   (final:ident)

-----

main state 66 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-110 => state 48   (final:ident)
111 => state 54   (final:ident)
112-122 => state 48   (final:ident)

-----

main state 67 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-113 => state 48   (final:ident)
114 => state 57   (final:ident)
115-122 => state 48   (final:ident)

-----

main state 68 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-114 => state 48   (final:ident)
115 => state 24   (final:ident)
116-122 => state 48   (final:ident)

-----

main state 69 (final:ident):

39 => state 48   (final:ident)
48-57 => state 48   (final:ident)
65-90 => state 48   (final:ident)
95 => state 48   (final:ident)
97-113 => state 48   (final:ident)
114 => state 42   (final:ident)
115-122 => state 48   (final:ident)

-----

main state 70 (final:number):

48-57 => state 70   (final:number)

-----

main state 71:

42 => state 12   (sink:entercommentblock)
47 => state 15   (sink:entercommentline)

-----

main state 72:

38 => state 7   (sink:land)

-----

main state 73:

61 => state 14   (sink:assign)

-----

main state 74:

61 => state 4   (sink:equal)

-----

main state 75:

124 => state 5   (sink:lor)

-----

main state 76:

48-57 => state 70   (final:number)

*)

struct
local
structure LexEngine = LexEngineFun (structure Streamable = Streamable
type symbol = Arg.symbol
val ord = Arg.ord)
structure Tables = struct
fun epsilon _ = raise (Fail "Illegal lexeme")
val commentblock = (7, 5, 8, Vector.fromList [Arg.commentblock_newline,Arg.entercommentblock,Arg.commentblock_newline,Arg.skip,Arg.eof,Arg.commentblock_newline,Arg.error,Arg.entercommentblock], LexEngine.next7x1 128 "\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^C\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^A\^B\^B\^F\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\b\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^D\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@", LexEngine.next0x1 "\^@\^@\^@\^@\^@\^@\^@\^E\^@")
val commentline = (6, 4, 6, Vector.fromList [Arg.skip_newline,Arg.entercommentline,Arg.skip_newline,Arg.eof,Arg.skip_newline,Arg.error], LexEngine.next7x1 128 "\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^C\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^A\^B\^B\^E\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B\^B", LexEngine.next0x1 "\^@\^@\^@\^@\^@\^@\^D")
val main = (21, 19, 70, Vector.fromList [Arg.lbrace,Arg.greaterequal,Arg.sub,Arg.equal,Arg.lor,Arg.add,Arg.land,Arg.rparen,Arg.semi,Arg.lessequal,Arg.skip_newline,Arg.entercommentblock,Arg.lparen,Arg.assign,Arg.entercommentline,Arg.mult,Arg.rbrace,Arg.notequal,Arg.eof,Arg.ident,Arg.error,Arg.ident,Arg.ident,Arg.ident,Arg.ident,Arg.ident,Arg.kwwhile,Arg.kwtrue,Arg.ident,Arg.kwelse,Arg.ident,Arg.ident,Arg.ident,Arg.ident,Arg.ident,Arg.skip,Arg.ident,Arg.continue,Arg.greater,Arg.ident,Arg.ident,Arg.ident,Arg.skip_newline,Arg.ident,Arg.ident,Arg.ident,Arg.ident,Arg.ident,Arg.lnot,Arg.ident,Arg.ident,Arg.ident,Arg.loop,Arg.ident,Arg.ident,Arg.return,Arg.ident,Arg.kwif,Arg.ident,Arg.ident,Arg.ident,Arg.less,Arg.break,Arg.ident,Arg.kwfalse,Arg.ident,Arg.ident,Arg.ident,Arg.ident,Arg.number], LexEngine.next7x1 128 "\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@00000000000000000\^Y00000000\^@\^@\^@\^@\^@$\^@\^@\^@\^@\^@\^@\^@\^@$\v\^@$+\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@$1\^@\^@\^@\^@H\^@\r\b\^P\^F\^@\^C\^@GFFFFFFFFFFI\t>J'\^@\^@00000000000000000000000000\^@\^@\^@\^@\^@\^@0C<0,=00\^W00#0000030\^T00!000\^AK\^QL\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@000000000000000000(0000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@00000:00000000000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@0000A000000000000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@00000000000000000000\"00000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@00000000000000000000-00000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@00000000000000000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@00000000000000000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@00000000)00000000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@00000000000000000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@0000000000000@000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@/0000000000000000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@0000000\^]000000000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@0000\^\000000000000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@00000000000000B00000000000\^@\^@\^@\^@\^@$\^@\^@\^@\^@\^@\^@\^@\^@$\^@\^@$\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@$\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@00000000000000000000E00000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@00000000000000000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^B\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@0000\^^000000000000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@00000000000200000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@00000000000008000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\v\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@00000000000\^V00000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@0000&000000000000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@0000000000000\^Z000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@0000000000?000000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@00000000000000000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^R\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@0000\^[000000000000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@00004000000000000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@0000000000000000000%000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@00000000000000000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@00000000000000050000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@00000000000D00000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@00000000000000000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@0000 000000000000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@00000000000000000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@00000000.00000000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@00000000000000\^_00000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@70000000000000000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\n\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@00000000000000000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@0000000000000000000;000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@00000000000000000000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@00000000000000600000000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@00000000000000000900000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@000000000000000000\^X0000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@0\^@\^@\^@\^@\^@\^@\^@\^@0000000000\^@\^@\^@\^@\^@\^@\^@00000000000000000000000000\^@\^@\^@\^@0\^@00000000000000000*00000000\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@FFFFFFFFFF\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\f\^@\^@\^@\^@\^O\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\a\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^N\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^D\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^E\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@FFFFFFFFFF\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@", LexEngine.next0x1 "\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^S\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@\^@")
end
in
fun commentblock s = LexEngine.lex {commentblock=commentblock, commentline=commentline, main=main} Tables.commentblock s
and commentline s = LexEngine.lex {commentblock=commentblock, commentline=commentline, main=main} Tables.commentline s
and main s = LexEngine.lex {commentblock=commentblock, commentline=commentline, main=main} Tables.main s
end
end
structure Lexer =
struct
  datatype token =
    ADD
  | SUB
  | MULT

  | NUMBER of int
  | IDENT of string
  | LPAREN
  | RPAREN

  | EQUAL
  | NOTEQUAL
  | LESS
  | LESSEQUAL
  | GREATER
  | GREATEREQUAL

  | TRUE
  | FALSE
  | OR
  | AND
  | NOT

  | ASSIGN
  | SEMI
  | IF
  | LBRACE
  | RBRACE
  | ELSE
  | LOOP
  | WHILE
  | BREAK
  | CONTINUE
  | RETURN


  open Stream

  exception LexError of string

  (* line number, character number *)
  type pos = int * int
  fun pos_to_string (l, c) =
    "line " ^ Int.toString l ^ ", column " ^ Int.toString c

  type t = pos -> (token * pos) front

  type self = {
    main : char stream -> t,
    commentline : char stream -> t,
    commentblock : char stream -> t
  }

  type info = {
    match : char list,
    len : int,
    start : char stream,
    follow : char stream,
    self : self
  }

  fun action f ({ match, len, follow, self, ... }:info) (l, c) =
     Cons (f (match, len, (l, c)), lazy (fn () => #main self follow (l, c+len)))

  fun simple token ({ len, follow, self, ... }:info) (l, c) =
     Cons ((token, (l, c)), lazy (fn () => #main self follow (l, c+len)))

  structure Arg =
  struct
    type symbol = char
    val ord = Char.ord

    type t = t

    type self = self
    type info = info

    fun eof _ _ = Nil

    fun skip ({ len, follow, self, ... }:info) (l, c) =
      #main self follow (l, c+len)

    fun skip_newline ({ len, follow, self, ... }:info) (l, c) =
      #main self follow (l+1, 0)

    val ident = action (fn (chars, _, pos) => (IDENT (implode chars), pos))

    val number =
      action
        (fn (chars, _, pos) =>
         (case Int.fromString (implode chars) of
            SOME n => (NUMBER n, pos)
          | NONE => raise Bind)
          handle Overflow =>
            raise LexError ("Illegal constant at " ^ pos_to_string pos))

    fun error _ pos =
      raise LexError ("Lexical error at " ^ pos_to_string pos)

    fun entercommentline ({len, follow, self, ...}:info) (l, c) =
      #commentline self follow (l, c+len)

    fun entercommentblock ({len, follow, self, ...}:info) (l, c) =
      #commentblock self follow (l, c+len)

    fun commentblock_newline ({len, follow, self, ...}:info) (l, c) =
      #commentblock self follow (l+1, 0)

    val add = simple ADD
    val sub = simple SUB
    val mult = simple MULT

    val lparen = simple LPAREN
    val rparen = simple RPAREN

    val equal = simple EQUAL
    val notequal = simple NOTEQUAL
    val less = simple LESS
    val lessequal = simple LESSEQUAL
    val greater = simple GREATER
    val greaterequal = simple GREATEREQUAL

    val kwtrue = simple TRUE
    val kwfalse = simple FALSE
    val land = simple AND
    val lor = simple OR
    val lnot = simple NOT

    val assign = simple ASSIGN
    val semi = simple SEMI
    val kwif = simple IF
    val lbrace = simple LBRACE
    val rbrace = simple RBRACE
    val kwelse = simple ELSE
    val loop = simple LOOP
    val kwwhile = simple WHILE
    val break = simple BREAK
    val continue = simple CONTINUE
    val return = simple RETURN

  end

  structure LexMain = CNotLexFun(
    structure Streamable = StreamStreamable
    structure Arg = Arg
  )

  fun lex s = lazy (fn () => LexMain.main s (1, 0))
end
(* File generated by CM-Yacc version 2.1 *)

functor CNotParseFun
   (structure Streamable : STREAMABLE
    structure Arg :
       sig
          type int
          type string
          type aexp
          type bexp
          type command
          type commandlist

          val cmds_cons : command * commandlist -> commandlist
          val cmds_one : command -> commandlist
          val cmds_empty : unit -> commandlist
          val cmds_id : commandlist -> commandlist
          val cmd_return : aexp -> command
          val cmd_continue : unit -> command
          val cmd_break : unit -> command
          val cmd_loop : commandlist -> command
          val cmd_if : bexp * commandlist * commandlist -> command
          val cmd_assign : string * aexp -> command
          val bexp_or : bexp * bexp -> bexp
          val bexp_and : bexp * bexp -> bexp
          val bexp_not : bexp -> bexp
          val bexp_greaterequal : aexp * aexp -> bexp
          val bexp_greater : aexp * aexp -> bexp
          val bexp_lessequal : aexp * aexp -> bexp
          val bexp_less : aexp * aexp -> bexp
          val bexp_notequal : aexp * aexp -> bexp
          val bexp_equal : aexp * aexp -> bexp
          val bexp_false : unit -> bexp
          val bexp_true : unit -> bexp
          val bexp_id : bexp -> bexp
          val aexp_mult : aexp * aexp -> aexp
          val aexp_sub : aexp * aexp -> aexp
          val aexp_add : aexp * aexp -> aexp
          val aexp_number : int -> aexp
          val aexp_ident : string -> aexp
          val aexp_id : aexp -> aexp

          datatype terminal =
             ADD
           | SUB
           | MULT
           | NUMBER of int
           | IDENT of string
           | LPAREN
           | RPAREN
           | EQUAL
           | NOTEQUAL
           | LESS
           | LESSEQUAL
           | GREATER
           | GREATEREQUAL
           | TRUE
           | FALSE
           | AND
           | OR
           | NOT
           | ASSIGN
           | SEMI
           | IF
           | LBRACE
           | RBRACE
           | ELSE
           | LOOP
           | WHILE
           | BREAK
           | CONTINUE
           | RETURN

          val error : terminal Streamable.t -> exn
       end)
   :>
   sig
      val parse : Arg.terminal Streamable.t -> Arg.commandlist * Arg.terminal Streamable.t
   end
=

(*

AUTOMATON LISTING
=================

State 0:

start -> . Cmds  / 0
18 : Cmd -> . IDENT ASSIGN Aexp SEMI  / 1
19 : Cmd -> . IF LPAREN Bexp RPAREN Block ELSE Block  / 1
20 : Cmd -> . LOOP Block  / 1
21 : Cmd -> . WHILE LPAREN TRUE RPAREN Block  / 1
22 : Cmd -> . BREAK SEMI  / 1
23 : Cmd -> . CONTINUE SEMI  / 1
24 : Cmd -> . RETURN Aexp SEMI  / 1
27 : Cmds -> . Cmd  / 0
28 : Cmds -> . Cmd Cmds  / 0

IDENT => shift 8
IF => shift 7
LOOP => shift 6
WHILE => shift 5
BREAK => shift 4
CONTINUE => shift 3
RETURN => shift 2
Cmd => goto 1
Cmds => goto 9

-----

State 1:

18 : Cmd -> . IDENT ASSIGN Aexp SEMI  / 2
19 : Cmd -> . IF LPAREN Bexp RPAREN Block ELSE Block  / 2
20 : Cmd -> . LOOP Block  / 2
21 : Cmd -> . WHILE LPAREN TRUE RPAREN Block  / 2
22 : Cmd -> . BREAK SEMI  / 2
23 : Cmd -> . CONTINUE SEMI  / 2
24 : Cmd -> . RETURN Aexp SEMI  / 2
27 : Cmds -> . Cmd  / 3
27 : Cmds -> Cmd .  / 3
28 : Cmds -> . Cmd Cmds  / 3
28 : Cmds -> Cmd . Cmds  / 3

$ => reduce 27
IDENT => shift 8
IF => shift 7
RBRACE => reduce 27
LOOP => shift 6
WHILE => shift 5
BREAK => shift 4
CONTINUE => shift 3
RETURN => shift 2
Cmd => goto 1
Cmds => goto 10

-----

State 2:

0 : Aexp -> . LPAREN Aexp RPAREN  / 4
1 : Aexp -> . IDENT  / 4
2 : Aexp -> . NUMBER  / 4
3 : Aexp -> . Aexp ADD Aexp  / 4
4 : Aexp -> . Aexp SUB Aexp  / 4
5 : Aexp -> . Aexp MULT Aexp  / 4
24 : Cmd -> RETURN . Aexp SEMI  / 2

NUMBER => shift 14
IDENT => shift 13
LPAREN => shift 12
Aexp => goto 11

-----

State 3:

23 : Cmd -> CONTINUE . SEMI  / 2

SEMI => shift 15

-----

State 4:

22 : Cmd -> BREAK . SEMI  / 2

SEMI => shift 16

-----

State 5:

21 : Cmd -> WHILE . LPAREN TRUE RPAREN Block  / 2

LPAREN => shift 17

-----

State 6:

20 : Cmd -> LOOP . Block  / 2
25 : Block -> . LBRACE Cmds RBRACE  / 2
26 : Block -> . LBRACE RBRACE  / 2

LBRACE => shift 18
Block => goto 19

-----

State 7:

19 : Cmd -> IF . LPAREN Bexp RPAREN Block ELSE Block  / 2

LPAREN => shift 20

-----

State 8:

18 : Cmd -> IDENT . ASSIGN Aexp SEMI  / 2

ASSIGN => shift 21

-----

State 9:

start -> Cmds .  / 0

$ => accept

-----

State 10:

28 : Cmds -> Cmd Cmds .  / 3

$ => reduce 28
RBRACE => reduce 28

-----

State 11:

3 : Aexp -> Aexp . ADD Aexp  / 4
4 : Aexp -> Aexp . SUB Aexp  / 4
5 : Aexp -> Aexp . MULT Aexp  / 4
24 : Cmd -> RETURN Aexp . SEMI  / 2

ADD => shift 25
SUB => shift 24
MULT => shift 23
SEMI => shift 22

-----

State 12:

0 : Aexp -> . LPAREN Aexp RPAREN  / 5
0 : Aexp -> LPAREN . Aexp RPAREN  / 6
1 : Aexp -> . IDENT  / 5
2 : Aexp -> . NUMBER  / 5
3 : Aexp -> . Aexp ADD Aexp  / 5
4 : Aexp -> . Aexp SUB Aexp  / 5
5 : Aexp -> . Aexp MULT Aexp  / 5

NUMBER => shift 14
IDENT => shift 13
LPAREN => shift 12
Aexp => goto 26

-----

State 13:

1 : Aexp -> IDENT .  / 6

ADD => reduce 1
SUB => reduce 1
MULT => reduce 1
RPAREN => reduce 1
EQUAL => reduce 1
NOTEQUAL => reduce 1
LESS => reduce 1
LESSEQUAL => reduce 1
GREATER => reduce 1
GREATEREQUAL => reduce 1
AND => reduce 1
OR => reduce 1
SEMI => reduce 1

-----

State 14:

2 : Aexp -> NUMBER .  / 6

ADD => reduce 2
SUB => reduce 2
MULT => reduce 2
RPAREN => reduce 2
EQUAL => reduce 2
NOTEQUAL => reduce 2
LESS => reduce 2
LESSEQUAL => reduce 2
GREATER => reduce 2
GREATEREQUAL => reduce 2
AND => reduce 2
OR => reduce 2
SEMI => reduce 2

-----

State 15:

23 : Cmd -> CONTINUE SEMI .  / 2

$ => reduce 23
IDENT => reduce 23
IF => reduce 23
RBRACE => reduce 23
LOOP => reduce 23
WHILE => reduce 23
BREAK => reduce 23
CONTINUE => reduce 23
RETURN => reduce 23

-----

State 16:

22 : Cmd -> BREAK SEMI .  / 2

$ => reduce 22
IDENT => reduce 22
IF => reduce 22
RBRACE => reduce 22
LOOP => reduce 22
WHILE => reduce 22
BREAK => reduce 22
CONTINUE => reduce 22
RETURN => reduce 22

-----

State 17:

21 : Cmd -> WHILE LPAREN . TRUE RPAREN Block  / 2

TRUE => shift 27

-----

State 18:

18 : Cmd -> . IDENT ASSIGN Aexp SEMI  / 7
19 : Cmd -> . IF LPAREN Bexp RPAREN Block ELSE Block  / 7
20 : Cmd -> . LOOP Block  / 7
21 : Cmd -> . WHILE LPAREN TRUE RPAREN Block  / 7
22 : Cmd -> . BREAK SEMI  / 7
23 : Cmd -> . CONTINUE SEMI  / 7
24 : Cmd -> . RETURN Aexp SEMI  / 7
25 : Block -> LBRACE . Cmds RBRACE  / 8
26 : Block -> LBRACE . RBRACE  / 8
27 : Cmds -> . Cmd  / 9
28 : Cmds -> . Cmd Cmds  / 9

IDENT => shift 8
IF => shift 7
RBRACE => shift 28
LOOP => shift 6
WHILE => shift 5
BREAK => shift 4
CONTINUE => shift 3
RETURN => shift 2
Cmd => goto 1
Cmds => goto 29

-----

State 19:

20 : Cmd -> LOOP Block .  / 2

$ => reduce 20
IDENT => reduce 20
IF => reduce 20
RBRACE => reduce 20
LOOP => reduce 20
WHILE => reduce 20
BREAK => reduce 20
CONTINUE => reduce 20
RETURN => reduce 20

-----

State 20:

0 : Aexp -> . LPAREN Aexp RPAREN  / 10
1 : Aexp -> . IDENT  / 10
2 : Aexp -> . NUMBER  / 10
3 : Aexp -> . Aexp ADD Aexp  / 10
4 : Aexp -> . Aexp SUB Aexp  / 10
5 : Aexp -> . Aexp MULT Aexp  / 10
6 : Bexp -> . LPAREN Bexp RPAREN  / 11
7 : Bexp -> . TRUE  / 11
8 : Bexp -> . FALSE  / 11
9 : Bexp -> . Aexp EQUAL Aexp  / 11
10 : Bexp -> . Aexp NOTEQUAL Aexp  / 11
11 : Bexp -> . Aexp LESS Aexp  / 11
12 : Bexp -> . Aexp LESSEQUAL Aexp  / 11
13 : Bexp -> . Aexp GREATER Aexp  / 11
14 : Bexp -> . Aexp GREATEREQUAL Aexp  / 11
15 : Bexp -> . NOT Bexp  / 11
16 : Bexp -> . Bexp AND Bexp  / 11
17 : Bexp -> . Bexp OR Bexp  / 11
19 : Cmd -> IF LPAREN . Bexp RPAREN Block ELSE Block  / 2

NUMBER => shift 14
IDENT => shift 13
LPAREN => shift 35
TRUE => shift 34
FALSE => shift 33
NOT => shift 32
Aexp => goto 31
Bexp => goto 30

-----

State 21:

0 : Aexp -> . LPAREN Aexp RPAREN  / 4
1 : Aexp -> . IDENT  / 4
2 : Aexp -> . NUMBER  / 4
3 : Aexp -> . Aexp ADD Aexp  / 4
4 : Aexp -> . Aexp SUB Aexp  / 4
5 : Aexp -> . Aexp MULT Aexp  / 4
18 : Cmd -> IDENT ASSIGN . Aexp SEMI  / 2

NUMBER => shift 14
IDENT => shift 13
LPAREN => shift 12
Aexp => goto 36

-----

State 22:

24 : Cmd -> RETURN Aexp SEMI .  / 2

$ => reduce 24
IDENT => reduce 24
IF => reduce 24
RBRACE => reduce 24
LOOP => reduce 24
WHILE => reduce 24
BREAK => reduce 24
CONTINUE => reduce 24
RETURN => reduce 24

-----

State 23:

0 : Aexp -> . LPAREN Aexp RPAREN  / 6
1 : Aexp -> . IDENT  / 6
2 : Aexp -> . NUMBER  / 6
3 : Aexp -> . Aexp ADD Aexp  / 6
4 : Aexp -> . Aexp SUB Aexp  / 6
5 : Aexp -> . Aexp MULT Aexp  / 6
5 : Aexp -> Aexp MULT . Aexp  / 6

NUMBER => shift 14
IDENT => shift 13
LPAREN => shift 12
Aexp => goto 37

-----

State 24:

0 : Aexp -> . LPAREN Aexp RPAREN  / 6
1 : Aexp -> . IDENT  / 6
2 : Aexp -> . NUMBER  / 6
3 : Aexp -> . Aexp ADD Aexp  / 6
4 : Aexp -> . Aexp SUB Aexp  / 6
4 : Aexp -> Aexp SUB . Aexp  / 6
5 : Aexp -> . Aexp MULT Aexp  / 6

NUMBER => shift 14
IDENT => shift 13
LPAREN => shift 12
Aexp => goto 38

-----

State 25:

0 : Aexp -> . LPAREN Aexp RPAREN  / 6
1 : Aexp -> . IDENT  / 6
2 : Aexp -> . NUMBER  / 6
3 : Aexp -> . Aexp ADD Aexp  / 6
3 : Aexp -> Aexp ADD . Aexp  / 6
4 : Aexp -> . Aexp SUB Aexp  / 6
5 : Aexp -> . Aexp MULT Aexp  / 6

NUMBER => shift 14
IDENT => shift 13
LPAREN => shift 12
Aexp => goto 39

-----

State 26:

0 : Aexp -> LPAREN Aexp . RPAREN  / 6
3 : Aexp -> Aexp . ADD Aexp  / 5
4 : Aexp -> Aexp . SUB Aexp  / 5
5 : Aexp -> Aexp . MULT Aexp  / 5

ADD => shift 25
SUB => shift 24
MULT => shift 23
RPAREN => shift 40

-----

State 27:

21 : Cmd -> WHILE LPAREN TRUE . RPAREN Block  / 2

RPAREN => shift 41

-----

State 28:

26 : Block -> LBRACE RBRACE .  / 8

$ => reduce 26
IDENT => reduce 26
IF => reduce 26
RBRACE => reduce 26
ELSE => reduce 26
LOOP => reduce 26
WHILE => reduce 26
BREAK => reduce 26
CONTINUE => reduce 26
RETURN => reduce 26

-----

State 29:

25 : Block -> LBRACE Cmds . RBRACE  / 8

RBRACE => shift 42

-----

State 30:

16 : Bexp -> Bexp . AND Bexp  / 11
17 : Bexp -> Bexp . OR Bexp  / 11
19 : Cmd -> IF LPAREN Bexp . RPAREN Block ELSE Block  / 2

RPAREN => shift 43
AND => shift 44
OR => shift 45

-----

State 31:

3 : Aexp -> Aexp . ADD Aexp  / 10
4 : Aexp -> Aexp . SUB Aexp  / 10
5 : Aexp -> Aexp . MULT Aexp  / 10
9 : Bexp -> Aexp . EQUAL Aexp  / 11
10 : Bexp -> Aexp . NOTEQUAL Aexp  / 11
11 : Bexp -> Aexp . LESS Aexp  / 11
12 : Bexp -> Aexp . LESSEQUAL Aexp  / 11
13 : Bexp -> Aexp . GREATER Aexp  / 11
14 : Bexp -> Aexp . GREATEREQUAL Aexp  / 11

ADD => shift 25
SUB => shift 24
MULT => shift 23
EQUAL => shift 51
NOTEQUAL => shift 50
LESS => shift 49
LESSEQUAL => shift 48
GREATER => shift 47
GREATEREQUAL => shift 46

-----

State 32:

0 : Aexp -> . LPAREN Aexp RPAREN  / 10
1 : Aexp -> . IDENT  / 10
2 : Aexp -> . NUMBER  / 10
3 : Aexp -> . Aexp ADD Aexp  / 10
4 : Aexp -> . Aexp SUB Aexp  / 10
5 : Aexp -> . Aexp MULT Aexp  / 10
6 : Bexp -> . LPAREN Bexp RPAREN  / 11
7 : Bexp -> . TRUE  / 11
8 : Bexp -> . FALSE  / 11
9 : Bexp -> . Aexp EQUAL Aexp  / 11
10 : Bexp -> . Aexp NOTEQUAL Aexp  / 11
11 : Bexp -> . Aexp LESS Aexp  / 11
12 : Bexp -> . Aexp LESSEQUAL Aexp  / 11
13 : Bexp -> . Aexp GREATER Aexp  / 11
14 : Bexp -> . Aexp GREATEREQUAL Aexp  / 11
15 : Bexp -> . NOT Bexp  / 11
15 : Bexp -> NOT . Bexp  / 11
16 : Bexp -> . Bexp AND Bexp  / 11
17 : Bexp -> . Bexp OR Bexp  / 11

NUMBER => shift 14
IDENT => shift 13
LPAREN => shift 35
TRUE => shift 34
FALSE => shift 33
NOT => shift 32
Aexp => goto 31
Bexp => goto 52

-----

State 33:

8 : Bexp -> FALSE .  / 11

RPAREN => reduce 8
AND => reduce 8
OR => reduce 8

-----

State 34:

7 : Bexp -> TRUE .  / 11

RPAREN => reduce 7
AND => reduce 7
OR => reduce 7

-----

State 35:

0 : Aexp -> . LPAREN Aexp RPAREN  / 12
0 : Aexp -> LPAREN . Aexp RPAREN  / 12
1 : Aexp -> . IDENT  / 12
2 : Aexp -> . NUMBER  / 12
3 : Aexp -> . Aexp ADD Aexp  / 12
4 : Aexp -> . Aexp SUB Aexp  / 12
5 : Aexp -> . Aexp MULT Aexp  / 12
6 : Bexp -> . LPAREN Bexp RPAREN  / 11
6 : Bexp -> LPAREN . Bexp RPAREN  / 11
7 : Bexp -> . TRUE  / 11
8 : Bexp -> . FALSE  / 11
9 : Bexp -> . Aexp EQUAL Aexp  / 11
10 : Bexp -> . Aexp NOTEQUAL Aexp  / 11
11 : Bexp -> . Aexp LESS Aexp  / 11
12 : Bexp -> . Aexp LESSEQUAL Aexp  / 11
13 : Bexp -> . Aexp GREATER Aexp  / 11
14 : Bexp -> . Aexp GREATEREQUAL Aexp  / 11
15 : Bexp -> . NOT Bexp  / 11
16 : Bexp -> . Bexp AND Bexp  / 11
17 : Bexp -> . Bexp OR Bexp  / 11

NUMBER => shift 14
IDENT => shift 13
LPAREN => shift 35
TRUE => shift 34
FALSE => shift 33
NOT => shift 32
Aexp => goto 54
Bexp => goto 53

-----

State 36:

3 : Aexp -> Aexp . ADD Aexp  / 4
4 : Aexp -> Aexp . SUB Aexp  / 4
5 : Aexp -> Aexp . MULT Aexp  / 4
18 : Cmd -> IDENT ASSIGN Aexp . SEMI  / 2

ADD => shift 25
SUB => shift 24
MULT => shift 23
SEMI => shift 55

-----

State 37:

3 : Aexp -> Aexp . ADD Aexp  / 6
4 : Aexp -> Aexp . SUB Aexp  / 6
5 : Aexp -> Aexp . MULT Aexp  / 6
5 : Aexp -> Aexp MULT Aexp .  / 6

ADD => reduce 5, shift 25  PRECEDENCE
SUB => reduce 5, shift 24  PRECEDENCE
MULT => shift 23, reduce 5  PRECEDENCE
RPAREN => reduce 5
EQUAL => reduce 5
NOTEQUAL => reduce 5
LESS => reduce 5
LESSEQUAL => reduce 5
GREATER => reduce 5
GREATEREQUAL => reduce 5
AND => reduce 5
OR => reduce 5
SEMI => reduce 5

-----

State 38:

3 : Aexp -> Aexp . ADD Aexp  / 6
4 : Aexp -> Aexp . SUB Aexp  / 6
4 : Aexp -> Aexp SUB Aexp .  / 6
5 : Aexp -> Aexp . MULT Aexp  / 6

ADD => shift 25, reduce 4  PRECEDENCE
SUB => shift 24, reduce 4  PRECEDENCE
MULT => shift 23, reduce 4  PRECEDENCE
RPAREN => reduce 4
EQUAL => reduce 4
NOTEQUAL => reduce 4
LESS => reduce 4
LESSEQUAL => reduce 4
GREATER => reduce 4
GREATEREQUAL => reduce 4
AND => reduce 4
OR => reduce 4
SEMI => reduce 4

-----

State 39:

3 : Aexp -> Aexp . ADD Aexp  / 6
3 : Aexp -> Aexp ADD Aexp .  / 6
4 : Aexp -> Aexp . SUB Aexp  / 6
5 : Aexp -> Aexp . MULT Aexp  / 6

ADD => shift 25, reduce 3  PRECEDENCE
SUB => shift 24, reduce 3  PRECEDENCE
MULT => shift 23, reduce 3  PRECEDENCE
RPAREN => reduce 3
EQUAL => reduce 3
NOTEQUAL => reduce 3
LESS => reduce 3
LESSEQUAL => reduce 3
GREATER => reduce 3
GREATEREQUAL => reduce 3
AND => reduce 3
OR => reduce 3
SEMI => reduce 3

-----

State 40:

0 : Aexp -> LPAREN Aexp RPAREN .  / 6

ADD => reduce 0
SUB => reduce 0
MULT => reduce 0
RPAREN => reduce 0
EQUAL => reduce 0
NOTEQUAL => reduce 0
LESS => reduce 0
LESSEQUAL => reduce 0
GREATER => reduce 0
GREATEREQUAL => reduce 0
AND => reduce 0
OR => reduce 0
SEMI => reduce 0

-----

State 41:

21 : Cmd -> WHILE LPAREN TRUE RPAREN . Block  / 2
25 : Block -> . LBRACE Cmds RBRACE  / 2
26 : Block -> . LBRACE RBRACE  / 2

LBRACE => shift 18
Block => goto 56

-----

State 42:

25 : Block -> LBRACE Cmds RBRACE .  / 8

$ => reduce 25
IDENT => reduce 25
IF => reduce 25
RBRACE => reduce 25
ELSE => reduce 25
LOOP => reduce 25
WHILE => reduce 25
BREAK => reduce 25
CONTINUE => reduce 25
RETURN => reduce 25

-----

State 43:

19 : Cmd -> IF LPAREN Bexp RPAREN . Block ELSE Block  / 2
25 : Block -> . LBRACE Cmds RBRACE  / 13
26 : Block -> . LBRACE RBRACE  / 13

LBRACE => shift 18
Block => goto 57

-----

State 44:

0 : Aexp -> . LPAREN Aexp RPAREN  / 10
1 : Aexp -> . IDENT  / 10
2 : Aexp -> . NUMBER  / 10
3 : Aexp -> . Aexp ADD Aexp  / 10
4 : Aexp -> . Aexp SUB Aexp  / 10
5 : Aexp -> . Aexp MULT Aexp  / 10
6 : Bexp -> . LPAREN Bexp RPAREN  / 11
7 : Bexp -> . TRUE  / 11
8 : Bexp -> . FALSE  / 11
9 : Bexp -> . Aexp EQUAL Aexp  / 11
10 : Bexp -> . Aexp NOTEQUAL Aexp  / 11
11 : Bexp -> . Aexp LESS Aexp  / 11
12 : Bexp -> . Aexp LESSEQUAL Aexp  / 11
13 : Bexp -> . Aexp GREATER Aexp  / 11
14 : Bexp -> . Aexp GREATEREQUAL Aexp  / 11
15 : Bexp -> . NOT Bexp  / 11
16 : Bexp -> . Bexp AND Bexp  / 11
16 : Bexp -> Bexp AND . Bexp  / 11
17 : Bexp -> . Bexp OR Bexp  / 11

NUMBER => shift 14
IDENT => shift 13
LPAREN => shift 35
TRUE => shift 34
FALSE => shift 33
NOT => shift 32
Aexp => goto 31
Bexp => goto 58

-----

State 45:

0 : Aexp -> . LPAREN Aexp RPAREN  / 10
1 : Aexp -> . IDENT  / 10
2 : Aexp -> . NUMBER  / 10
3 : Aexp -> . Aexp ADD Aexp  / 10
4 : Aexp -> . Aexp SUB Aexp  / 10
5 : Aexp -> . Aexp MULT Aexp  / 10
6 : Bexp -> . LPAREN Bexp RPAREN  / 11
7 : Bexp -> . TRUE  / 11
8 : Bexp -> . FALSE  / 11
9 : Bexp -> . Aexp EQUAL Aexp  / 11
10 : Bexp -> . Aexp NOTEQUAL Aexp  / 11
11 : Bexp -> . Aexp LESS Aexp  / 11
12 : Bexp -> . Aexp LESSEQUAL Aexp  / 11
13 : Bexp -> . Aexp GREATER Aexp  / 11
14 : Bexp -> . Aexp GREATEREQUAL Aexp  / 11
15 : Bexp -> . NOT Bexp  / 11
16 : Bexp -> . Bexp AND Bexp  / 11
17 : Bexp -> . Bexp OR Bexp  / 11
17 : Bexp -> Bexp OR . Bexp  / 11

NUMBER => shift 14
IDENT => shift 13
LPAREN => shift 35
TRUE => shift 34
FALSE => shift 33
NOT => shift 32
Aexp => goto 31
Bexp => goto 59

-----

State 46:

0 : Aexp -> . LPAREN Aexp RPAREN  / 14
1 : Aexp -> . IDENT  / 14
2 : Aexp -> . NUMBER  / 14
3 : Aexp -> . Aexp ADD Aexp  / 14
4 : Aexp -> . Aexp SUB Aexp  / 14
5 : Aexp -> . Aexp MULT Aexp  / 14
14 : Bexp -> Aexp GREATEREQUAL . Aexp  / 11

NUMBER => shift 14
IDENT => shift 13
LPAREN => shift 12
Aexp => goto 60

-----

State 47:

0 : Aexp -> . LPAREN Aexp RPAREN  / 14
1 : Aexp -> . IDENT  / 14
2 : Aexp -> . NUMBER  / 14
3 : Aexp -> . Aexp ADD Aexp  / 14
4 : Aexp -> . Aexp SUB Aexp  / 14
5 : Aexp -> . Aexp MULT Aexp  / 14
13 : Bexp -> Aexp GREATER . Aexp  / 11

NUMBER => shift 14
IDENT => shift 13
LPAREN => shift 12
Aexp => goto 61

-----

State 48:

0 : Aexp -> . LPAREN Aexp RPAREN  / 14
1 : Aexp -> . IDENT  / 14
2 : Aexp -> . NUMBER  / 14
3 : Aexp -> . Aexp ADD Aexp  / 14
4 : Aexp -> . Aexp SUB Aexp  / 14
5 : Aexp -> . Aexp MULT Aexp  / 14
12 : Bexp -> Aexp LESSEQUAL . Aexp  / 11

NUMBER => shift 14
IDENT => shift 13
LPAREN => shift 12
Aexp => goto 62

-----

State 49:

0 : Aexp -> . LPAREN Aexp RPAREN  / 14
1 : Aexp -> . IDENT  / 14
2 : Aexp -> . NUMBER  / 14
3 : Aexp -> . Aexp ADD Aexp  / 14
4 : Aexp -> . Aexp SUB Aexp  / 14
5 : Aexp -> . Aexp MULT Aexp  / 14
11 : Bexp -> Aexp LESS . Aexp  / 11

NUMBER => shift 14
IDENT => shift 13
LPAREN => shift 12
Aexp => goto 63

-----

State 50:

0 : Aexp -> . LPAREN Aexp RPAREN  / 14
1 : Aexp -> . IDENT  / 14
2 : Aexp -> . NUMBER  / 14
3 : Aexp -> . Aexp ADD Aexp  / 14
4 : Aexp -> . Aexp SUB Aexp  / 14
5 : Aexp -> . Aexp MULT Aexp  / 14
10 : Bexp -> Aexp NOTEQUAL . Aexp  / 11

NUMBER => shift 14
IDENT => shift 13
LPAREN => shift 12
Aexp => goto 64

-----

State 51:

0 : Aexp -> . LPAREN Aexp RPAREN  / 14
1 : Aexp -> . IDENT  / 14
2 : Aexp -> . NUMBER  / 14
3 : Aexp -> . Aexp ADD Aexp  / 14
4 : Aexp -> . Aexp SUB Aexp  / 14
5 : Aexp -> . Aexp MULT Aexp  / 14
9 : Bexp -> Aexp EQUAL . Aexp  / 11

NUMBER => shift 14
IDENT => shift 13
LPAREN => shift 12
Aexp => goto 65

-----

State 52:

15 : Bexp -> NOT Bexp .  / 11
16 : Bexp -> Bexp . AND Bexp  / 11
17 : Bexp -> Bexp . OR Bexp  / 11

RPAREN => reduce 15
AND => reduce 15, shift 44  PRECEDENCE
OR => reduce 15, shift 45  PRECEDENCE

-----

State 53:

6 : Bexp -> LPAREN Bexp . RPAREN  / 11
16 : Bexp -> Bexp . AND Bexp  / 11
17 : Bexp -> Bexp . OR Bexp  / 11

RPAREN => shift 66
AND => shift 44
OR => shift 45

-----

State 54:

0 : Aexp -> LPAREN Aexp . RPAREN  / 12
3 : Aexp -> Aexp . ADD Aexp  / 12
4 : Aexp -> Aexp . SUB Aexp  / 12
5 : Aexp -> Aexp . MULT Aexp  / 12
9 : Bexp -> Aexp . EQUAL Aexp  / 11
10 : Bexp -> Aexp . NOTEQUAL Aexp  / 11
11 : Bexp -> Aexp . LESS Aexp  / 11
12 : Bexp -> Aexp . LESSEQUAL Aexp  / 11
13 : Bexp -> Aexp . GREATER Aexp  / 11
14 : Bexp -> Aexp . GREATEREQUAL Aexp  / 11

ADD => shift 25
SUB => shift 24
MULT => shift 23
RPAREN => shift 40
EQUAL => shift 51
NOTEQUAL => shift 50
LESS => shift 49
LESSEQUAL => shift 48
GREATER => shift 47
GREATEREQUAL => shift 46

-----

State 55:

18 : Cmd -> IDENT ASSIGN Aexp SEMI .  / 2

$ => reduce 18
IDENT => reduce 18
IF => reduce 18
RBRACE => reduce 18
LOOP => reduce 18
WHILE => reduce 18
BREAK => reduce 18
CONTINUE => reduce 18
RETURN => reduce 18

-----

State 56:

21 : Cmd -> WHILE LPAREN TRUE RPAREN Block .  / 2

$ => reduce 21
IDENT => reduce 21
IF => reduce 21
RBRACE => reduce 21
LOOP => reduce 21
WHILE => reduce 21
BREAK => reduce 21
CONTINUE => reduce 21
RETURN => reduce 21

-----

State 57:

19 : Cmd -> IF LPAREN Bexp RPAREN Block . ELSE Block  / 2

ELSE => shift 67

-----

State 58:

16 : Bexp -> Bexp . AND Bexp  / 11
16 : Bexp -> Bexp AND Bexp .  / 11
17 : Bexp -> Bexp . OR Bexp  / 11

RPAREN => reduce 16
AND => reduce 16, shift 44  PRECEDENCE
OR => reduce 16, shift 45  PRECEDENCE

-----

State 59:

16 : Bexp -> Bexp . AND Bexp  / 11
17 : Bexp -> Bexp . OR Bexp  / 11
17 : Bexp -> Bexp OR Bexp .  / 11

RPAREN => reduce 17
AND => shift 44, reduce 17  PRECEDENCE
OR => reduce 17, shift 45  PRECEDENCE

-----

State 60:

3 : Aexp -> Aexp . ADD Aexp  / 14
4 : Aexp -> Aexp . SUB Aexp  / 14
5 : Aexp -> Aexp . MULT Aexp  / 14
14 : Bexp -> Aexp GREATEREQUAL Aexp .  / 11

ADD => shift 25
SUB => shift 24
MULT => shift 23
RPAREN => reduce 14
AND => reduce 14
OR => reduce 14

-----

State 61:

3 : Aexp -> Aexp . ADD Aexp  / 14
4 : Aexp -> Aexp . SUB Aexp  / 14
5 : Aexp -> Aexp . MULT Aexp  / 14
13 : Bexp -> Aexp GREATER Aexp .  / 11

ADD => shift 25
SUB => shift 24
MULT => shift 23
RPAREN => reduce 13
AND => reduce 13
OR => reduce 13

-----

State 62:

3 : Aexp -> Aexp . ADD Aexp  / 14
4 : Aexp -> Aexp . SUB Aexp  / 14
5 : Aexp -> Aexp . MULT Aexp  / 14
12 : Bexp -> Aexp LESSEQUAL Aexp .  / 11

ADD => shift 25
SUB => shift 24
MULT => shift 23
RPAREN => reduce 12
AND => reduce 12
OR => reduce 12

-----

State 63:

3 : Aexp -> Aexp . ADD Aexp  / 14
4 : Aexp -> Aexp . SUB Aexp  / 14
5 : Aexp -> Aexp . MULT Aexp  / 14
11 : Bexp -> Aexp LESS Aexp .  / 11

ADD => shift 25
SUB => shift 24
MULT => shift 23
RPAREN => reduce 11
AND => reduce 11
OR => reduce 11

-----

State 64:

3 : Aexp -> Aexp . ADD Aexp  / 14
4 : Aexp -> Aexp . SUB Aexp  / 14
5 : Aexp -> Aexp . MULT Aexp  / 14
10 : Bexp -> Aexp NOTEQUAL Aexp .  / 11

ADD => shift 25
SUB => shift 24
MULT => shift 23
RPAREN => reduce 10
AND => reduce 10
OR => reduce 10

-----

State 65:

3 : Aexp -> Aexp . ADD Aexp  / 14
4 : Aexp -> Aexp . SUB Aexp  / 14
5 : Aexp -> Aexp . MULT Aexp  / 14
9 : Bexp -> Aexp EQUAL Aexp .  / 11

ADD => shift 25
SUB => shift 24
MULT => shift 23
RPAREN => reduce 9
AND => reduce 9
OR => reduce 9

-----

State 66:

6 : Bexp -> LPAREN Bexp RPAREN .  / 11

RPAREN => reduce 6
AND => reduce 6
OR => reduce 6

-----

State 67:

19 : Cmd -> IF LPAREN Bexp RPAREN Block ELSE . Block  / 2
25 : Block -> . LBRACE Cmds RBRACE  / 2
26 : Block -> . LBRACE RBRACE  / 2

LBRACE => shift 18
Block => goto 68

-----

State 68:

19 : Cmd -> IF LPAREN Bexp RPAREN Block ELSE Block .  / 2

$ => reduce 19
IDENT => reduce 19
IF => reduce 19
RBRACE => reduce 19
LOOP => reduce 19
WHILE => reduce 19
BREAK => reduce 19
CONTINUE => reduce 19
RETURN => reduce 19

-----

lookahead 0 = $
lookahead 1 = $ IDENT IF LOOP WHILE BREAK CONTINUE RETURN
lookahead 2 = $ IDENT IF RBRACE LOOP WHILE BREAK CONTINUE RETURN
lookahead 3 = $ RBRACE
lookahead 4 = ADD SUB MULT SEMI
lookahead 5 = ADD SUB MULT RPAREN
lookahead 6 = ADD SUB MULT RPAREN EQUAL NOTEQUAL LESS LESSEQUAL GREATER GREATEREQUAL AND OR SEMI
lookahead 7 = IDENT IF RBRACE LOOP WHILE BREAK CONTINUE RETURN
lookahead 8 = $ IDENT IF RBRACE ELSE LOOP WHILE BREAK CONTINUE RETURN
lookahead 9 = RBRACE
lookahead 10 = ADD SUB MULT EQUAL NOTEQUAL LESS LESSEQUAL GREATER GREATEREQUAL
lookahead 11 = RPAREN AND OR
lookahead 12 = ADD SUB MULT RPAREN EQUAL NOTEQUAL LESS LESSEQUAL GREATER GREATEREQUAL
lookahead 13 = ELSE
lookahead 14 = ADD SUB MULT RPAREN AND OR

*)

struct
local
structure Value = struct
datatype nonterminal =
nonterminal
| int of Arg.int
| string of Arg.string
| aexp of Arg.aexp
| bexp of Arg.bexp
| command of Arg.command
| commandlist of Arg.commandlist
end
structure ParseEngine = ParseEngineFun (structure Streamable = Streamable
type terminal = Arg.terminal
type value = Value.nonterminal
val dummy = Value.nonterminal
fun read terminal =
(case terminal of
Arg.ADD => (1, Value.nonterminal)
| Arg.SUB => (2, Value.nonterminal)
| Arg.MULT => (3, Value.nonterminal)
| Arg.NUMBER x => (4, Value.int x)
| Arg.IDENT x => (5, Value.string x)
| Arg.LPAREN => (6, Value.nonterminal)
| Arg.RPAREN => (7, Value.nonterminal)
| Arg.EQUAL => (8, Value.nonterminal)
| Arg.NOTEQUAL => (9, Value.nonterminal)
| Arg.LESS => (10, Value.nonterminal)
| Arg.LESSEQUAL => (11, Value.nonterminal)
| Arg.GREATER => (12, Value.nonterminal)
| Arg.GREATEREQUAL => (13, Value.nonterminal)
| Arg.TRUE => (14, Value.nonterminal)
| Arg.FALSE => (15, Value.nonterminal)
| Arg.AND => (16, Value.nonterminal)
| Arg.OR => (17, Value.nonterminal)
| Arg.NOT => (18, Value.nonterminal)
| Arg.ASSIGN => (19, Value.nonterminal)
| Arg.SEMI => (20, Value.nonterminal)
| Arg.IF => (21, Value.nonterminal)
| Arg.LBRACE => (22, Value.nonterminal)
| Arg.RBRACE => (23, Value.nonterminal)
| Arg.ELSE => (24, Value.nonterminal)
| Arg.LOOP => (25, Value.nonterminal)
| Arg.WHILE => (26, Value.nonterminal)
| Arg.BREAK => (27, Value.nonterminal)
| Arg.CONTINUE => (28, Value.nonterminal)
| Arg.RETURN => (29, Value.nonterminal)
)
)
in
val parse = ParseEngine.parse (
ParseEngine.next5x1 "\128\128\128\128\128\137\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\136\128\128\128\135\134\133\132\131\128\128c\128\128\128\128\137\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\136\128c\128\135\134\133\132\131\128\128\128\128\128\128\143\142\141\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\144\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\145\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\146\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\147\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\149\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\150\128\128\128\128\128\128\128\128\128\128\128\128\127\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128b\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128b\128\128\128\128\128\128\128\128\128\154\153\152\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\151\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\143\142\141\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128}}}\128\128\128}}}}}}}\128\128}}\128\128}\128\128\128\128\128\128\128\128\128\128\128\128|||\128\128\128|||||||\128\128||\128\128|\128\128\128\128\128\128\128\128\128\128\128g\128\128\128\128g\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128g\128g\128ggggg\128\128h\128\128\128\128h\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128h\128h\128hhhhh\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\156\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\137\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\136\128\157\128\135\134\133\132\131\128\128j\128\128\128\128j\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128j\128j\128jjjjj\128\128\128\128\128\128\143\142\164\128\128\128\128\128\128\128\163\162\128\128\161\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\143\142\141\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128f\128\128\128\128f\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128f\128f\128fffff\128\128\128\128\128\128\143\142\141\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\143\142\141\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\143\142\141\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\154\153\152\128\128\128\169\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\170\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128d\128\128\128\128d\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128d\128ddddddd\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\171\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\172\128\128\128\128\128\128\128\128\173\174\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\154\153\152\128\128\128\128\180\179\178\177\176\175\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\143\142\164\128\128\128\128\128\128\128\163\162\128\128\161\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128v\128\128\128\128\128\128\128\128vv\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128w\128\128\128\128\128\128\128\128ww\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\143\142\164\128\128\128\128\128\128\128\163\162\128\128\161\128\128\128\128\128\128\128\128\128\128\128\128\128\128\154\153\152\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\184\128\128\128\128\128\128\128\128\128\128\128\128yy\152\128\128\128yyyyyyy\128\128yy\128\128y\128\128\128\128\128\128\128\128\128\128\128\128\154\153\152\128\128\128zzzzzzz\128\128zz\128\128z\128\128\128\128\128\128\128\128\128\128\128\128\154\153\152\128\128\128{{{{{{{\128\128{{\128\128{\128\128\128\128\128\128\128\128\128\128\128\128~~~\128\128\128~~~~~~~\128\128~~\128\128~\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\147\128\128\128\128\128\128\128\128\128e\128\128\128\128e\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128e\128eeeeeee\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\147\128\128\128\128\128\128\128\128\128\128\128\128\128\143\142\164\128\128\128\128\128\128\128\163\162\128\128\161\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\143\142\164\128\128\128\128\128\128\128\163\162\128\128\161\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\143\142\141\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\143\142\141\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\143\142\141\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\143\142\141\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\143\142\141\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\143\142\141\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128o\128\128\128\128\128\128\128\128oo\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\195\128\128\128\128\128\128\128\128\173\174\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\154\153\152\128\128\128\169\180\179\178\177\176\175\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128l\128\128\128\128l\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128l\128l\128lllll\128\128i\128\128\128\128i\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128i\128i\128iiiii\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\196\128\128\128\128\128\128\128\128\128\128\128\128\128\128n\128\128\128\128\128\128\128\128nn\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128m\128\128\128\128\128\128\128\128\173m\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\154\153\152\128\128\128p\128\128\128\128\128\128\128\128pp\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\154\153\152\128\128\128q\128\128\128\128\128\128\128\128qq\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\154\153\152\128\128\128r\128\128\128\128\128\128\128\128rr\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\154\153\152\128\128\128s\128\128\128\128\128\128\128\128ss\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\154\153\152\128\128\128t\128\128\128\128\128\128\128\128tt\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\154\153\152\128\128\128u\128\128\128\128\128\128\128\128uu\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128x\128\128\128\128\128\128\128\128xx\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\147\128\128\128\128\128\128\128\128\128k\128\128\128\128k\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128k\128k\128kkkkk\128\128",
ParseEngine.next5x1 "\128\128\129\128\137\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\129\128\138\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\139\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\147\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\154\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\129\128\157\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\159\158\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\164\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\165\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\166\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\167\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\159\180\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\182\181\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\184\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\185\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\159\186\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\159\187\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\188\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\189\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\190\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\191\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\192\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\193\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\196\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128\128",
Vector.fromList [(0,3,(fn _::Value.aexp(arg0)::_::rest => Value.aexp(Arg.aexp_id arg0)::rest|_=>raise (Fail "bad parser"))),
(0,1,(fn Value.string(arg0)::rest => Value.aexp(Arg.aexp_ident arg0)::rest|_=>raise (Fail "bad parser"))),
(0,1,(fn Value.int(arg0)::rest => Value.aexp(Arg.aexp_number arg0)::rest|_=>raise (Fail "bad parser"))),
(0,3,(fn Value.aexp(arg0)::_::Value.aexp(arg1)::rest => Value.aexp(Arg.aexp_add {2=arg0,1=arg1})::rest|_=>raise (Fail "bad parser"))),
(0,3,(fn Value.aexp(arg0)::_::Value.aexp(arg1)::rest => Value.aexp(Arg.aexp_sub {2=arg0,1=arg1})::rest|_=>raise (Fail "bad parser"))),
(0,3,(fn Value.aexp(arg0)::_::Value.aexp(arg1)::rest => Value.aexp(Arg.aexp_mult {2=arg0,1=arg1})::rest|_=>raise (Fail "bad parser"))),
(1,3,(fn _::Value.bexp(arg0)::_::rest => Value.bexp(Arg.bexp_id arg0)::rest|_=>raise (Fail "bad parser"))),
(1,1,(fn _::rest => Value.bexp(Arg.bexp_true {})::rest|_=>raise (Fail "bad parser"))),
(1,1,(fn _::rest => Value.bexp(Arg.bexp_false {})::rest|_=>raise (Fail "bad parser"))),
(1,3,(fn Value.aexp(arg0)::_::Value.aexp(arg1)::rest => Value.bexp(Arg.bexp_equal {2=arg0,1=arg1})::rest|_=>raise (Fail "bad parser"))),
(1,3,(fn Value.aexp(arg0)::_::Value.aexp(arg1)::rest => Value.bexp(Arg.bexp_notequal {2=arg0,1=arg1})::rest|_=>raise (Fail "bad parser"))),
(1,3,(fn Value.aexp(arg0)::_::Value.aexp(arg1)::rest => Value.bexp(Arg.bexp_less {2=arg0,1=arg1})::rest|_=>raise (Fail "bad parser"))),
(1,3,(fn Value.aexp(arg0)::_::Value.aexp(arg1)::rest => Value.bexp(Arg.bexp_lessequal {2=arg0,1=arg1})::rest|_=>raise (Fail "bad parser"))),
(1,3,(fn Value.aexp(arg0)::_::Value.aexp(arg1)::rest => Value.bexp(Arg.bexp_greater {2=arg0,1=arg1})::rest|_=>raise (Fail "bad parser"))),
(1,3,(fn Value.aexp(arg0)::_::Value.aexp(arg1)::rest => Value.bexp(Arg.bexp_greaterequal {2=arg0,1=arg1})::rest|_=>raise (Fail "bad parser"))),
(1,2,(fn Value.bexp(arg0)::_::rest => Value.bexp(Arg.bexp_not arg0)::rest|_=>raise (Fail "bad parser"))),
(1,3,(fn Value.bexp(arg0)::_::Value.bexp(arg1)::rest => Value.bexp(Arg.bexp_and {2=arg0,1=arg1})::rest|_=>raise (Fail "bad parser"))),
(1,3,(fn Value.bexp(arg0)::_::Value.bexp(arg1)::rest => Value.bexp(Arg.bexp_or {2=arg0,1=arg1})::rest|_=>raise (Fail "bad parser"))),
(2,4,(fn _::Value.aexp(arg0)::_::Value.string(arg1)::rest => Value.command(Arg.cmd_assign {2=arg0,1=arg1})::rest|_=>raise (Fail "bad parser"))),
(2,7,(fn Value.commandlist(arg0)::_::Value.commandlist(arg1)::_::Value.bexp(arg2)::_::_::rest => Value.command(Arg.cmd_if {3=arg0,2=arg1,1=arg2})::rest|_=>raise (Fail "bad parser"))),
(2,2,(fn Value.commandlist(arg0)::_::rest => Value.command(Arg.cmd_loop arg0)::rest|_=>raise (Fail "bad parser"))),
(2,5,(fn Value.commandlist(arg0)::_::_::_::_::rest => Value.command(Arg.cmd_loop arg0)::rest|_=>raise (Fail "bad parser"))),
(2,2,(fn _::_::rest => Value.command(Arg.cmd_break {})::rest|_=>raise (Fail "bad parser"))),
(2,2,(fn _::_::rest => Value.command(Arg.cmd_continue {})::rest|_=>raise (Fail "bad parser"))),
(2,3,(fn _::Value.aexp(arg0)::_::rest => Value.command(Arg.cmd_return arg0)::rest|_=>raise (Fail "bad parser"))),
(3,3,(fn _::Value.commandlist(arg0)::_::rest => Value.commandlist(Arg.cmds_id arg0)::rest|_=>raise (Fail "bad parser"))),
(3,2,(fn _::_::rest => Value.commandlist(Arg.cmds_empty {})::rest|_=>raise (Fail "bad parser"))),
(4,1,(fn Value.command(arg0)::rest => Value.commandlist(Arg.cmds_one arg0)::rest|_=>raise (Fail "bad parser"))),
(4,2,(fn Value.commandlist(arg0)::Value.command(arg1)::rest => Value.commandlist(Arg.cmds_cons {2=arg0,1=arg1})::rest|_=>raise (Fail "bad parser")))],
(fn Value.commandlist x => x | _ => raise (Fail "bad parser")), Arg.error)
end
end

in

signature INTERPRETER =
  sig
    datatype aexp =
      AConst of int
    | Variable of string
    | Add of aexp * aexp
    | Sub of aexp * aexp
    | Mult of aexp * aexp

    datatype bexp =
      BConst of bool
    | Equals of aexp * aexp
    | NotEquals of aexp * aexp
    | LessThan of aexp * aexp
    | LessThanEqual of aexp * aexp
    | GreaterThan of aexp * aexp
    | GreaterThanEqual of aexp * aexp
    | Not of bexp
    | And of bexp * bexp
    | Or of bexp * bexp

    datatype command =
      Assign of string * aexp
    | If of bexp * command list * command list
    | Loop of command list
    | Break
    | Continue
    | Return of aexp

    type program = command list

    type environment = (string * int) list

    val runCommand  : environment -> command -> environment
    val runCommands : environment -> command list -> environment
    val runProgram  : command list -> int
  end

functor Utils (include INTERPRETER) =
   struct
      local
         structure Parser =
         struct
           exception ParseError of string

           structure Arg =
           struct
             type string = string
             type int = int
             type aexp = aexp
             type bexp = bexp
             type command = command
             type commandlist = command list
             type program = program
             type identlist = string list

             fun singleton s = [s]

             val aexp_id = Fn.id
             val aexp_ident = Variable
             val aexp_number = AConst
             val aexp_add = Add
             val aexp_sub = Sub
             val aexp_mult = Mult

             val bexp_id = Fn.id
             fun bexp_true () = BConst true
             fun bexp_false () = BConst false
             val bexp_equal = Equals
             val bexp_notequal = NotEquals
             val bexp_less = LessThan
             val bexp_lessequal = LessThanEqual
             val bexp_greater = GreaterThan
             val bexp_greaterequal = GreaterThanEqual
             val bexp_not = Not
             val bexp_and = And
             val bexp_or = Or

             val cmd_assign = Assign
             val cmd_if = If
             val cmd_loop = Loop
             fun cmd_break () = Break
             fun cmd_continue () = Continue
             val cmd_return = Return

             val cmds_id = Fn.id
             fun cmds_empty () = []
             fun cmds_one x = [x]
             val cmds_cons = op::

             datatype terminal = datatype Lexer.token

             fun error s =
               case Stream.front s of
                 Stream.Nil => ParseError "Syntax error at end of file."
               | Stream.Cons ((_, (l, c)), _) =>
                   ParseError ("Syntax error at line " ^ Int.toString l ^ ", column " ^ Int.toString c ^ ".\n")
           end

           (* line number, character number *)
           type pos = int * int

           structure StreamWithPos =
              CoercedStreamable (structure Streamable = StreamStreamable
                                 type 'a item = 'a * pos
                                 fun coerce (x, _) = x)

           structure ParseMain = CNotParseFun (
             structure Streamable = StreamWithPos
             structure Arg = Arg
           )

           val parse = #1 o ParseMain.parse o Lexer.lex
         end

         fun parseFile (file : string) : command list =
           let
             val ins = TextIO.openIn file
           in
             Parser.parse (Stream.fromTextInstream ins)
             before
             TextIO.closeIn ins
           end

         val parseString : string -> command list = Parser.parse o Stream.fromString
      in
         exception LexError = Lexer.LexError
         exception ParseError = Parser.ParseError
         val parseFile : string -> command list = parseFile
         val parseString : string -> command list = parseString
      end
      local
        infixr $
        fun f $ x = f x

        infix |>
        fun x |> f = f x

        open TextIO
        open OS.FileSys

        val timeoutSeconds = ref (Time.fromSeconds 1)
        exception TimeoutError

        val printList = print o String.concat

        structure TerminalCodes =
        struct
          fun put code = String.concat ["\027[", code, ""]

          datatype color = RED | GREEN | YELLOW
          val colorCode = fn
              RED => "31m"
            | GREEN => "32m"
            | YELLOW => "33m"

          datatype style = BOLD | BLINKING
          val styleCode = fn
              BOLD => "1m"
            | BLINKING => "5m"

          datatype action = CLEAR_LINE
          val actionCode = fn
              CLEAR_LINE => "2K"

          val normalCode = "0m"

          val putColor = put o colorCode
          val putStyle = put o styleCode
          val putAction = put o actionCode
          val putNormal = put normalCode

          fun good s = putColor GREEN ^ putStyle BOLD ^ s ^ putNormal
          fun warn s = putColor YELLOW ^ s ^ putNormal
          fun bad s = putColor RED ^ putStyle BOLD ^ s ^ putNormal
          fun bold s = putStyle BOLD ^ s ^ putNormal
          fun blinking s = putStyle BLINKING ^ s ^ putNormal
        end
        open TerminalCodes

        local
          val padded = ref false
        in
          fun startPad () =
            let
              val padding = if !padded then "" else "\n"
            in
              padded := false; padding
            end
          fun endPad () = (padded := true; "\n")
          fun resetPad () = (padded := false; "")
        end

        fun listFiles dirName =
          let
            val dir = openDir dirName
            fun listFiles' acc =
              case readDir dir of
                   SOME file =>
                      (case OS.Path.ext file of
                           SOME "cnot" => listFiles' (file::acc)
                         | _ => (printList [startPad (),
                                            warn "Ignoring file: ", file, "\n    ",
                                            "It doesn't seem to be a .cnot file\n",
                                            endPad ()];
                                 listFiles' acc))
                 | NONE => acc
            val result = listFiles' []
            val () = closeDir dir
          in
            result
          end

        fun lines s = String.tokens (fn c => c = #"\n") s

        fun readEntireFile testDir path =
          let
            val stream = openIn (OS.Path.concat (testDir,path))
            val contents = inputAll stream
            val () = closeIn stream
          in
            contents
          end

        exception TestError of string

        fun test testDir testName =
          let
            val text = readEntireFile testDir testName
            val firstLine =
              case lines text of
                   first::_ => first
                 | [] => raise TestError ("Test '" ^ testName ^ "' does not have a first line\n")
            (* yum valOf *)
            val expected = Option.valOf o Int.fromString $ String.extract (firstLine, 2, NONE)
              handle _ (* extract, valOf *) => raise TestError ("Failed to parse expected result from first line of test '" ^ testName ^ "'")

            fun indent _ [] = []
              | indent _ [x] = [x]
              | indent s (x::xs) = x::s::indent s xs

            datatype 'a result = OK of 'a | ERR of exn

            (* inspired by autograder, which is inspired by msullivan *)
            fun evalWithTimeout (f: 'a -> 'b) (x: 'a) =
              case SMLofNJ.SysInfo.getOSKind () of
                SMLofNJ.SysInfo.WIN32 => f x
              | _ =>
                    let
                      val retVal = ref NONE
                      (* SML/NJ Interval Timer *)
                      val timer = SMLofNJ.IntervalTimer.setIntTimer
                      fun finally () = (
                        (* Disable sigALRM *)
                        timer NONE;
                        Signals.setHandler (Signals.sigALRM, Signals.IGNORE);
                        ()
                      )
                      (* calling k x equivalent to returning x *)
                      fun run (k: unit SMLofNJ.Cont.cont) =
                      let
                        (* HANDLER (signal, num of times, continuation) *)
                        (* I tried Cont.throw k NONE, but the repl gave up after
                        * the first run. I think it got stuck in the signal
                        * handler, and you cannot invoke a signal handler in
                        * another one*)
                        val _ = Signals.setHandler
                          (Signals.sigALRM, Signals.HANDLER (fn (s, n', k') => k))
                        (* SML/NJ send sigALRM signal every x seconds *)
                        val _ = timer (SOME (!timeoutSeconds))
                        val result: 'b = f x
                      in
                        retVal := SOME result; ()
                      end
                      (* first class reified continuations, fancy *)
                      val _ = SMLofNJ.Cont.callcc run before (finally ())
                            handle e => (finally (); raise e)
                    in
                      case !retVal of
                          SOME x => x
                        | NONE => raise TimeoutError
                    end

            val testingMessage = String.concat [bold "Testing: ", testName, "..."]
            val _ = print testingMessage

            val result =
              OK $ evalWithTimeout (runProgram o parseFile) (OS.Path.concat (testDir,testName))
              handle e => ERR e

            val _ = printList [putAction CLEAR_LINE, "\r"]

            val () =
               case result of
                   OK res =>
                       if expected = res
                       then printList [good "Test Passed: ", testName, "\n", resetPad ()]
                       else printList [startPad (),
                                       bad "Test Failed: ", testName, "\n    ",
                                       bold "Expected result: ", Int.toString expected, "\n    ",
                                       bold "Got result: ", Int.toString res, "\n",
                                       endPad ()]
                 | ERR (LexError s) =>
                     printList [startPad (),
                                 bad "Lexical error in ", testName, ":\n",
                                 "      ", s, "\n",
                                 endPad ()]
                 | ERR (ParseError s) =>
                     printList [startPad (),
                                 bad "Syntax error in ", testName, ":\n",
                                 "      ", s, "\n",
                                 endPad ()]
                 | ERR TimeoutError =>
                     printList [startPad (), bad "Test Failed: ", testName, "\n    ",
                                 putStyle BOLD, "Execution timed out after ",
                                 Time.toString $ !timeoutSeconds, " second(s)\n",
                                 putNormal, endPad ()]
                 | ERR e =>
                     let
                       val raiseSite::handlerSites = List.rev $ SMLofNJ.exnHistory e

                       fun splitBy f [] = ([], [])
                         | splitBy f (x::xs) =
                           case (f x, splitBy f xs) of
                                 (true, (L, R)) => (x::L, R)
                               | (false, _) => ([], x::xs)

                       fun group ([]: string list) = []
                         | group (x::xs) =
                           let
                             val (L, R) = splitBy (fn y => y = x) xs
                           in
                             (x::L)::group R
                           end

                       (* If an exception passed through the same
                       * handle site 3+ times consecutively, collect the
                       * reports into a single line to avoid vomiting the
                       * same line onto the screen tons of times.
                       *
                       * Mostly shows up with buggy loop implementations
                       *)
                       val handlerSites =
                             handlerSites
                         (* We don't want to report the handle site in utils.sml *)
                         |> (fn L => List.take (L, List.length L - 1))
                         |> group
                         |> map (fn L =>
                                   case L of
                                       x::y::z::xs => [
                                         String.concat [x, "  [Repeated ",
                                                         Int.toString $ List.length L,
                                                         " times]"],
                                         "..."]
                                     | _ => L)
                         |> List.concat
                     in
                       printList ([startPad (),
                                   bad "Test Failed: ", testName, "\n    ",
                                   bold "Expected result: ", Int.toString expected, "\n    ",
                                   bold "Got uncaught exception: ", exnMessage e,
                                   "\n                raised at  ", raiseSite, "\n"]
                                 @ (case handlerSites of
                                         [] => []
                                       | _ => ["\n      handled/reraised at  "]
                                           @ indent "\n                           " handlerSites
                                           @ ["\n"])
                                 @ [endPad ()])
                     end
          in
            case result of
              OK _  => true
            | ERR _ => false
          end

        fun printErrors f x = f x
         handle TestError e => false before printList [blinking $ bad "TEST HARNESS ERROR: ", e, "\n"]
      in
        fun testAll testDir =
          let
            val tests = listFiles testDir
          in
            List.map (fn file => (file, printErrors (test testDir) file)) tests
          end

        fun setTimeoutSeconds s = timeoutSeconds := Time.fromSeconds (IntInf.fromInt s)
      end
   end

end
