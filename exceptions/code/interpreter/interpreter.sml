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

exception UnitializedVariable of string

(* set: string -> int -> environment -> environment
 * REQUIRES: true
 * ENSURES: set x n env ~= env' where
 *                          - If x was in env, its value is updated to n
 *                          - If x was not in env, it is created with the
 *                            initial value n
 *)
fun set (x : string) (v : int) : environment -> environment = fn
  [] => [(x, v)]
| (y, w)::es => if y = x then (x, v)::es else (y, w):: set x v es

(* get: environment -> string -> int option
 * REQUIRES: true
 * ENSURES: get env x ~= SOME n if x is in env, where n is the value of x
 *                       NONE if x is not in env
 *)
fun get (env : environment) (x : string) : int =
  case List.find (fn (y, _) => x = y) env of
    NONE => raise UnitializedVariable x
  | SOME (_, v) => v

val emptyEnvironment = []

(* evalAexp: environment -> aexp -> int
 * REQUIRES: every variable used in ae is initialized in env
 * ENSURES: evalAexp env ae ~= the result of evaluating the arithmetic
 *                             expression ae with the environment env
 *)
fun evalAexp (env : environment) :  aexp -> int = fn
    AConst n => n
  | Variable x => get env x
  | Add (e1, e2) => evalAexp env e1 + evalAexp env e2
  | Sub (e1, e2) => evalAexp env e1 - evalAexp env e2
  | Mult (e1, e2) => evalAexp env e1 * evalAexp env e2

(* evalBexp: environment -> bexp -> bool
 * REQUIRES: every variable used in be is initialized in env
 * ENSURES: evalAexp env be ~= the result of evaluating the boolean
 *                             expression be with the environment env
 *)
fun evalBexp (env : environment) : bexp -> bool = fn
    BConst b => b
  | Equals (e1, e2) => evalAexp env e1 = evalAexp env e2
  | NotEquals (e1, e2) => evalAexp env e1 <> evalAexp env e2
  | LessThan (e1, e2) => evalAexp env e1 < evalAexp env e2
  | LessThanEqual (e1, e2) => evalAexp env e1 <= evalAexp env e2
  | GreaterThan (e1, e2) => evalAexp env e1 > evalAexp env e2
  | GreaterThanEqual (e1, e2) => evalAexp env e1 >= evalAexp env e2
  | Not e => not (evalBexp env e)
  | And (e1, e2) => evalBexp env e1 andalso evalBexp env e2
  | Or (e1, e2) => evalBexp env e1 orelse evalBexp env e2

exception BreakExn of environment
exception ContinueExn of environment
exception ReturnExn of int

(* runCommand: environment -> command -> environment
 * REQUIRES: runCommand will only be invoked during the interpretation of a
 *           well-formed Cnot program
 * ENSURES: runCommand env cmd either
 *            - Evaluates to env', where env' is the result of updating env
 *              according to cmd
 *            - Raises an exception as specified in the handout
 *)
fun runCommand (env : environment) : command -> environment = fn
  Assign (s,a) => set s (evalAexp env a) env
| If (b,c1,c2) => if (evalBexp env b) then runCommands env c1 else runCommands env c2
| Loop (c)     => ((runCommands env (c @ [(Loop c)])) handle BreakExn e => e | ContinueExn e => runCommand e (Loop c))
| Break        => raise BreakExn env
| Continue     => raise ContinueExn env
| Return a     => raise ReturnExn (evalAexp env a)



and runCommands env cmds = foldl (fn (c, e) => runCommand e c) env cmds

(* runProgram: comamnd list -> int
 * REQUIRES: the input program is well-formed
 * ENSURES: runProgram cmds ~= the value returned by the program cmds
 *)
fun runProgram (cmds : command list) : int =
  let
    val _ = runCommands emptyEnvironment cmds
  in
    raise Fail "exited without return"
  end
  handle ReturnExn n => n
