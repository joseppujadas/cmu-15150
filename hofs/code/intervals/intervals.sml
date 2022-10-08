type interval = int * int

fun removeContainmennts(e:interval, [] : interval list) = [e] : interval list
 |  removeContainmennts((s1,e1):interval,(s2,e2)::xs : interval list) =
  if s1 > s2 andalso e1 < e2 then (s2,e2)::xs
  else (s1,e1)::(s2,e2)::xs

fun combineFold((s1,e1):interval, [] : interval list) : interval list = []
  | combineFold((s1,e1):interval, (s2,e2)::xs : interval list) : interval list =
  if e2 < s1 then (s1,e1)::(e2,s1)::xs
  else (s1,e1)::xs

fun intervalCompare((s1,e1): interval, (s2,e2) : interval) : order =
  case Int.compare(e1,e2) of
    LESS => GREATER
  | GREATER => LESS
  | _ => case Int.compare(s1,s2) of
    LESS => GREATER
  | GREATER => LESS
  | _ => EQUAL
  

(* all_available: interval list list -> interval list
 * REQUIRES:
 * ENSURES:
 *)
fun all_available (L : interval list list) : interval list =
  let
    (* val x = foldl updateFrees [(0,0)] (msort intervalCompare (List.concat L)) *)
    val x = foldl removeContainmennts [] (msort intervalCompare (List.concat L))
    val y = foldl combineFold [(0,0)] x
  in
    case y of
      [] => []
  |   z::zs => zs
  end
