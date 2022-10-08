type 'a key = ('a -> exn) * (exn -> 'a option)

(* new : unit -> 'a key
 * REQUIRES: true
 * ENSURES: new () ==> k such that (decrypt k o encrypt k) = SOME
 *          and for all k' /= k we have
 *          (decrypt k' o encrypt k) = (decrypt k o encrypt k') = fn _ => NONE
 *)
fun new () : 'a key =
let
  exception secret of 'a

in
  (secret, fn secret k => SOME k | _ => NONE)
end

(* encrypt : 'a key -> 'a -> exn
 * REQUIRES: true
 * ENSURES: If encrypt k x ==> y then decrypt k y ==> SOME x
 *)
fun encrypt ((enc, _) : 'a key) = enc

(* decrypt : 'a key -> exn -> 'a option
 * REQUIRES: true
 * ENSURES: decrypt k x ==> SOME y   if encrypt k y evaluated to x
 *                          NONE     otherwise
 *)
fun decrypt ((_, dec) : 'a key) = dec
