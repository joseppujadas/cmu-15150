datatype prop = Const of bool
              | Var of string
              | And of prop * prop
              | Or of prop * prop
              | Not of prop

type assignment = string * bool


(* subst : assignment * prop -> prop
 * REQUIRES: true
 * ENSURES: subst ((c, b), p) ==>* p' such that
 *  - p' does not contain c
 *  - p' is equivalent to p except all instances of c have been replaced with
 *    True if b is true and False if b is false.
 *)
fun subst ((c, b), Var c') = if c = c' then Const b else Var c'
  | subst ((c, b), And (p1, p2)) = And (subst ((c, b), p1), subst ((c, b), p2))
  | subst ((c, b), Or (p1, p2)) = Or (subst ((c, b), p1), subst ((c, b), p2))
  | subst ((c, b), Not p) = Not (subst ((c, b), p))
  | subst (_, p) = p

(* subst_all : prop -> assignment list -> prop
 * REQUIRES: all strings in asgns are unique
 * ENSURES: subst_all p asgns evaluates to a proposition with all
 *  instances of variables in asgns replaced with the Const True or Const False,
 *  according to their corresponding booleans in asgns.
 *)
val subst_all = foldl subst

(* try_eval : prop -> (bool -> 'a) -> (string -> 'a) -> 'a
 * REQUIRES: true
 * ENSURES: if p contains no variables,
 *  then try_eval p sc fc ==>* sc b where b is the truth value of p.
 *  Otherwise, try_eval p sc fc ==>* fc c where c is the leftmost variable in p.
 *)
fun try_eval (Const x : prop) (sc : bool -> 'a) (fc : string -> 'a) : 'a = sc x
  | try_eval (Var s) _ fc = fc s
  | try_eval (And (p,q)) sc fc = try_eval (p) (fn res => try_eval (q) (fn res2 => sc(res andalso res2)) fc) fc
  | try_eval (Or (p,q)) sc fc = try_eval (p) (fn res => try_eval (q) (fn res2 => sc(res orelse res2)) fc) fc
  | try_eval (Not p) sc fc = try_eval p (fn res => sc (not res)) fc


(* sat : prop -> (assignment list -> 'a) -> (unit -> 'a) -> 'a
 * REQUIRES: true
 * ENSURES: if there exists some value asgns such that
 *  try_eval (subst_all p asgns) (fn x => x) (fn _ => false) ==>* true,
 *  then sat p s k ==>* s asgns.
 *  Otherwise, sat p s k ==>* k ().
 *)
fun sat (p : prop) (sc : assignment list -> 'a) (fc : unit -> 'a) : 'a =
  let
    fun fail (v : string) = sat (subst ((v,true), p)) (fn al => sc((v, true)::al)) (fn () => sat (subst ((v,false),p)) (fn al =>sc((v, false)::al)) fc)
  in
    try_eval p (fn true => sc [] | false => fc ()) fail
  end
