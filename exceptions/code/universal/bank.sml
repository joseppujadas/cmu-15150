type vault = exn list


(* get      : 'a key -> vault -> 'a option
 * REQUIRES : true
 * ENSURES  : get k M ==> SOME y   if there exists x in M such that
 *                                    decrypt k x ==> SOME y
 *                        NONE     otherwise
 *)
fun get (k : 'a key) ([] : vault) : 'a option = NONE
  | get (k : 'a key) (x::xs : vault) =
  case decrypt k x of
    SOME y => SOME y
  | NONE => get k xs

(* replace  : 'a key -> 'a -> vault -> 'a option * vault
 * REQUIRES : true
 * ENSURES  : replace k v M ==> (SOME y, M')   if there exists x in M such that
 *                                                decrypt k x = SOME y and M'
 *                                                contains x' = encrypt k v
 *                                                instead of x
 *                              (NONE, M)      otherwise
 *)
fun replace (k : 'a key) (v : 'a) ([] : vault) : 'a option * vault = (NONE, [])
  | replace (k : 'a key) (v : 'a) (x::xs : vault) : 'a option * vault =
    case decrypt k x of
      SOME y => (SOME y, (encrypt k v)::xs)
    | NONE   => let val (opt, rest) = replace k v xs in (opt,x::rest) end


(* deposit  : 'a key -> 'a -> vault -> vault
 * REQUIRES : true
 * ENSURES  : deposit k v M ==>  M' such that M' contains x = encrypt k v and
 *              no other values x' = vault k v' (for any value v)
 *)
fun deposit (k : 'a key) (v : 'a) (M : vault) : vault =
  case get k M of
    NONE => (encrypt k v)::M
  | SOME x => let val (opt, m) = replace k v M in m end

(* withdraw : 'a key -> vault -> vault
 * REQUIRES : true
 * ENSURES  : withdraw k M ==> M' where M' contains all elements x of M
 *              such that decrypt k x ==> NONE (and no others)
 *)
fun withdraw (k : 'a key) ([] : vault) : vault = []
  | withdraw (k : 'a key) (x::xs : vault) = 
  case decrypt k x of
    NONE => x::(withdraw k xs)
  | SOME y => xs
