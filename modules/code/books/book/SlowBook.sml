structure SlowBook :> BOOK =
struct
  type page = string
  datatype direction = FORWARD | BACKWARD

  exception OutOfBounds

  type book = page list * int

  (* bind : page list -> book *)
  fun bind L = (L, 0) (* bookmark starts at beginning *)

  (* unbind : book -> page list *)
  fun unbind (B, _) = B

  (* currentPage : book -> bool * page option *)
  local
    fun beginning 0 = true
      | beginning _ = false
    fun getPage (B, n) =
      if n < List.length B then SOME (List.nth (B, n)) else NONE
  in
    fun currentPage (B, n) = (beginning n, getPage (B, n))
  end

  (* flip : direction -> book -> book *)
  fun flip d (B, n) =
    case d of
      FORWARD =>
        if n >= List.length B then raise OutOfBounds
        else (B, n+1)
    | BACKWARD =>
        if n <= 0 then raise OutOfBounds
        else (B, n-1)

end
