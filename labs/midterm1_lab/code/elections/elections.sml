datatype vote = A | B
type election = vote list

(* result : election -> int *)
(* REQUIRES : true *)
(* ENSURES : result e ==> s, the sum of votes for A in e - sum of votes
for B in e *)
fun result ([] : election) : int = 0
  | result (e::es : election) =
    case e of
      A =>  1 + result es
    | B => ~1 + result es
(* Test cases *)

val 0 = result []
val 1 = result [A]
val ~1 = result [B]
val 0 = result [A,B]
val 2 = result [A,A]
val ~1 = result [B,B,A]
val ~4 = result [B,B,B,B,A,A,B,B]

fun AW_help ([] : election, _) : bool = true
  | AW_help (e::es : election, x) : bool =
    if x < 0 then false
    else case e of
      A => AW_help(es, x+1)
    | B => AW_help(es, x-1)



(* always_winning : election -> bool *)
(* REQUIRES : result e > 0 *)
(* ENSURES : always_winning e ==> true iff at any gives point in counting
the votes from left to right, there are strictly more votes for A than
there are for B *)
fun always_winning (e : election) = AW_help(e,0)



(* Test cases *)

val true = always_winning [A]
val false = always_winning [B,A,A]
val true = always_winning [A,A,B,A,B]


(* election_map : election list * vote -> election list *)
(* REQUIRES : true *)
(* ENSURES : election_map (E,v) ==> L, an election list that is the same as E
but has the vote v prepended to all elections in E *)
fun election_map ([] : election list, v : vote) : election list = []
  | election_map (e::es : election list, v : vote) =
    (v::e) :: election_map(es,v)

(* Test cases *)

val [] = election_map([], A)
val [[A]] = election_map([[]], A)
val [[B,A],[B],[B,A,A]] = election_map([[A],[],[A,A]], B)


(* all_elections : int -> election list *)
(* REQUIRES : n >= 0 *)
(* ENSURES : all_elections n ==> L, a list of all elections with n votes *)
fun all_elections (0 : int) : election list = [[]]
  | all_elections (n : int) : election list =
    (election_map(all_elections(n-1), A)) @ (election_map(all_elections(n-1),B))

(* Test cases *)

val [[]] = all_elections 0
val [[A],[B]] = all_elections 1
val [[A,A],[A,B],[B,A],[B,B]] = all_elections 2
val [[A,A,A],[A,A,B],[A,B,A],[A,B,B],
    [B,A,A],[B,A,B],[B,B,A],[B,B,B]] = all_elections 3


(* all_perms : int * int -> election list *)
(* REQUIRES : a, b >= 0 *)
(* ENSURES : all_perms (a,b) ==> L, a list of all elections which have a true
             votes and b false votes *)
fun all_perms (a : int, b : int) : election list = 

(* Test cases *)

val [[]] = all_perms (0,0)
val [[A]] = all_perms (1,0)
val [[A,B],[B,A]] = all_perms(1,1)
val [[A,B,B],[B,A,B],[B,B,A]]= all_perms(1,2)
val [[A,A,A,A]] = all_perms(4,0)


(* winning : int * int -> election list *)
(* REQUIRES : a > b *)
(* ENSURES : winning (a,b) ==> L, a list of all of the elections where A wins by
receiving a votes and B receives b votes and there are always more votes for A
than there are for B *)
fun winning (a : int, b : int) : election list = raise Fail "Unimplemented"

(* Test cases *)
(*
val [[A]] = winning (1,0)
val [[A,A,A,A]] = winning (4,0)
val [[A,A,B]] = winning (2,1)
*)
