(* sum : int list -> int
 * REQUIRES: true
 * ENSURES: sum [x1, ..., xn] = x1 + ... + xn
 *)
val sum : int list -> int = foldr op+ 0


(* won'tStarve :
 *   int ->
 *   int ->
 *   int list ->
 *   (int list list -> 'a) ->
 *   (unit -> 'a) ->
 *   'a
 * REQUIRES: pots >= 0 and time >= 0 and for all x in curries, x > 0
 * ENSURES: won'tStarve pots time curries sc fc ==>
 *  sc D if there exists D such that concat D is a permutation of curries
 *  and length D = pots and for all d in D, sum d <= time.
 *  fc () otherwise.
 *)
fun won'tStarve
  (0 : int)
  (time : int)
  ([] : int list)
  (sc : int list list -> 'a)
  (fc : unit -> 'a)
  : 'a = sc []
  | won'tStarve pots time curries sc fc =
  let
    fun pL L scL fcL = if sum L <= time then scL L else fcL ()
    fun pR R scR fcR = if sum R <= (time * (pots-1)) then scR R else fcR ()
    fun scFP (l,r) = won'tStarve (pots-1) time r (fn D => sc(l::D)) fc
  in
    findPartition curries pL pR scFP fc
  end


(* (more than one possible answer here) *)
val sc = SOME
val fc = fn () => NONE
(* val SOME [[10, 40], [20], [30]] = won'tStarve 3 50 [10, 20, 30, 40] sc fc
val NONE = won'tStarve 2 40 [20, 20, 30, 40] sc fc *)
