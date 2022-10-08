(* findPartitionV1 :
 *  'e list ->
 *  ('e list -> 'l option) ->
 *  ('e list -> 'r option) ->
 *  ('l * 'r) option
 * REQUIRES: pL and pR are total
 * ENSURES: findPartitionV1 A pL pR ==>*
 *  SOME (evL, evR) if there exists a partition (L, R) of A
 *  such that pL accepts L with evL and pR accepts R with evR.
 *  NONE otherwise.
 *)


fun findPartitionV1 ([] : 'e list) (pL : 'e list -> 'l option) (pR : 'e list -> 'r option) : ('l * 'r) option =
  let
    val (evL, evR) = (pL [], pR [])
  in
    case (evL, evR) of
       (SOME l, SOME r) => SOME (l,r)
    |  _ => NONE
  end
  | findPartitionV1 (x::xs : 'e list) pL pR =
  let
    val tryLeft  = findPartitionV1 xs (fn l => pL(x::l)) pR
    val tryRight = findPartitionV1 xs pL (fn r => pR(x::r))
  in
    case tryLeft of
      SOME (evL, evR) => tryLeft
    | _ => case tryRight of
      SOME (evL, evR) => tryRight
    | _ => NONE
  end

val sum = foldr op+ 0
fun subsetSumV1 (n : int) (xs : int list) : int list option =
  let
    val pL = fn L => if sum L = n then SOME L else NONE
    val pR = fn R => SOME R
    val res = findPartitionV1 xs pL pR
    fun extractL (SOME (partL, partR)) = SOME partL
      | extractL NONE = NONE
  in
    extractL res
  end
(* findPartitionV2 :
 *  'e list ->
 *  ('e list -> 'l option) ->
 *  ('e list -> 'r option) ->
 *  ('l * 'r -> 'a) ->
 *  (unit -> 'a) ->
 *  'a
 * REQUIRES: pL and pR are total
 * ENSURES: findPartitionV2 A pL pR sc fc ==>*
 *  sc (evL, evR) if there exists a partition (L, R) of A
 *  such that pL accepts L with evL and pR accepts R with evR.
 *  fc () otherwise.
 *)
fun findPartitionV2 ([] : 'e list) (pL : 'e list -> 'l option) (pR : 'e list -> 'r option) (sc : 'l * 'r -> 'a) (fc : unit -> 'a) : 'a =
  let val (evL, evR) = (pL [], pR []) in
    case (evL, evR) of
       (SOME l, SOME r) => sc (l,r)
    |  _ => fc ()
  end
 |  findPartitionV2 (x::xs) pL pR sc fc = findPartitionV2 xs (fn l => pL(x::l)) pR sc (fn () => findPartitionV2 xs pL (fn r => pR(x::r)) sc fc)

fun subsetSumV2 (n : int) (xs : int list) : int list option =
  let
    val pL = fn L => if sum L = n then SOME L else NONE
    val pR = fn R => SOME R
    val sc = fn (l,r) => SOME l
    val fc = fn () => NONE
  in
    findPartitionV2 xs pL pR sc fc
  end

(* findPartition :
 *  'e list ->
 *  ('e list -> ('l -> 'a) -> (unit -> 'a) -> 'a) ->
 *  ('e list -> ('r -> 'a) -> (unit -> 'a) -> 'a) ->
 *  ('l * 'r -> 'a) ->
 *  (unit -> 'a) ->
 *  'a
 * REQUIRES: pL and pR are CPS-total
 * ENSURES: findPartition A pL pR sc fc ==>
 *  sc (evL, evR) if there exists a partition (L, R) of A
 *  such that pL accepts L with evL and pR accepts R with evR.
 *  fc () otherwise.
 *)
fun findPartition
  ([] : 'e list)
  (pL : 'e list -> ('l -> 'a) -> (unit -> 'a) -> 'a)
  (pR : 'e list -> ('r -> 'a) -> (unit -> 'a) -> 'a)
  (sc : 'l * 'r -> 'a)
  (fc : unit -> 'a)
  : 'a = pL [] (fn resL => pR [] (fn resR => sc (resL,resR)) fc) fc
  | findPartition (x::xs) pL pR sc fc = findPartition xs (fn l => pL(x::l)) pR sc (fn () => findPartition xs pL (fn r => pR(x::r)) sc fc)


val sum = foldr op+ 0

fun subsetSum (n : int) (xs : int list) : int list option =
  let
    fun pL L scL fcL = if sum L = n then scL L else fcL ()
    fun pR R scR fcR = scR ()
    fun sc (LJ, ()) = SOME LJ
    fun fc () = NONE
  in
    findPartition xs pL pR sc fc
  end

val SOME [] = subsetSum 0 []
val NONE = subsetSum 1 []
val SOME [1] = subsetSum 1 [1]
val SOME [1,1] = subsetSum 2 [1,1]
val SOME [1,2] = subsetSum 3 [1,2,1]
val SOME [2,1] = subsetSum 3 [5,2,1]
