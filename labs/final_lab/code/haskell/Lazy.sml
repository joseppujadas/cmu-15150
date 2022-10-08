structure Lazy :> LAZY =
struct
  datatype 'a cell =
      Thunk of unit -> 'a
    | Done of 'a
  type 'a t = 'a cell ref

  fun delay comp = ref (Thunk comp)

  fun force comp =
    case !comp of
      Thunk c =>
        let val result = c () in
        (comp := Done result; result) end
    | Done r => r
end
