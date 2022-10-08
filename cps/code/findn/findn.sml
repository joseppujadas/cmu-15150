datatype 'a shrub = Leaf of 'a
                  | Branch of 'a shrub * 'a shrub

(* findOne : ('a -> bool) -> 'a shrub -> ('a -> 'b) -> (unit -> 'b) -> 'b
 * REQUIRES: p is total
 * ENSURES:  findOne p T sc fc ==> sc x if x is in T and p x ==> true.
 *           If no such x exists, this evalutes to fc () instead.
 *           If more than one such x exists,
 *           findOne p T sc fc evaluates the leftmost such x.
 *)
fun findOne (p : 'a -> bool) (Leaf (x) : 'a shrub)
            (sc : 'a -> 'b) (fc : unit -> 'b) : 'b =
    if p x then sc x else fc ()
 |  findOne (p) (Branch(L,R)) (sc) (fc) =
      findOne p L (sc) (fn () => findOne p R sc fc)


(* findTwo : ('a -> bool) -> ('a * 'a -> bool) -> 'a shrub
 *           -> ('a * 'a -> 'b) -> (unit -> 'b) -> 'b
 * REQUIRES: p is total, eq is total, eq represents an equivalence relation
 * ENSURES:  findTwo p eq T sc fc ==> sc (x, y) if x and y are values in T s.t.
 *           p x ==> true and p y ==> true and eq (x,y) ==> false.
 *           If no such pair (x,y) exists, findTwo p T sc fc ==> fc ()
 *)
fun findTwo (p : 'a -> bool) (eq : 'a * 'a -> bool) (T : 'a shrub)
            (sc : 'a * 'a -> 'b) (fc : unit -> 'b) : 'b  =
    findOne p T (fn res => (findOne (fn x => p x andalso not (eq(res,x))) (T) (fn res2 => sc(res,res2)) (fc))) fc


(* findN : ('a -> bool) -> ('a * 'a -> bool) -> 'a shrub -> int
 *         -> ('a list -> 'b) -> (unit -> 'b) -> 'b
 * REQUIRES: n >= 0, p is total, eq is total,
 *           eq represents an equivalence relation
 * ENSURES: findN p eq T n sc fc ==> sc L if there exists
 *          a list L of length n s.t the elements in L are pairwise distinct
 *          by eq, and for each element x in L, p x ==> true.
 *          Otherwise evaluates to fc ().
 *)
fun findN (p : 'a -> bool) (eq : 'a * 'a -> bool) (T : 'a shrub)
          (n : int) (sc : 'a list -> 'b) (fc : unit -> 'b) =
  case n of
    0 => sc []
  | _ =>
      let
        fun success x = findN (fn y => (p y andalso not (eq(y,x)))) eq (T) (n-1) (fn z => sc(x::z)) fc
      in
        findOne p T success fc
      end
