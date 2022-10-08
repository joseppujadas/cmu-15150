exception InvalidToken
exception InvalidExpression

datatype token =
    Integer of int
  | Multiply
  | Add
  | Subtract
  | Divide
  | SumAll

fun tokenize "*" = Multiply
  | tokenize "+" = Add
  | tokenize "-" = Subtract
  | tokenize "/" = Divide
  | tokenize "sum" = SumAll
  | tokenize numberString =
      case Int.fromString numberString of
        SOME x => Integer x
      | NONE => raise InvalidToken

(* We can't write the usual `raise Fail "Unimplemented!"` because these are val
 * bindings, not fun bindings (the exception would be raised right away).
 * Instead, we use `const : 'a -> 'b -> 'a` to make our stubs *)
fun const a b = a

(* Pay careful attention to the writeup's restrictions here. For example, if you
 * turn these `val`s to `fun`s, you'll probably lose points. You can only write one
 * non-recursive `fun` helper function for `eval` *)

(* parse : string -> token list *)
val parse : string -> token list =
  map tokenize o (String.tokens (fn #" " => true | _ => false) )

fun doOp (Integer x : token, stack : int list) : int list = x :: stack
  | doOp (SumAll, stack) = [foldl (op +) 0 stack]
  | doOp (oper, [])  = raise InvalidExpression
  | doOp (oper, [x]) = raise InvalidExpression
  | doOp (oper, x::y::stack) =
    case oper of
      Multiply  => (x * y)   :: stack
    | Add       => (x + y)   :: stack
    | Subtract  => (y - x)   :: stack
    | Divide    => (y div x) :: stack
    | _ => raise InvalidExpression

(* eval : token list -> int list *)
val eval : token list -> int list = rev o foldl doOp []

(* rpn : string -> int list *)
val rpn : string -> int list = eval o parse
