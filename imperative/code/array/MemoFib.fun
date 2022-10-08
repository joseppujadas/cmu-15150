functor MemoFib (ArrayFn : ARRAYFN) :> MEMOFIB =
struct
  (* memo_fib: unit -> int -> int
   * REQUIRES: i >= 0
   * ENSURES: see spec in PDF
   *)
  fun memo_fib () =
    let

      val lookup = ref (ArrayFn.new ~1 2)
      val () = (ArrayFn.nth (!lookup) 0 := 0; ArrayFn.nth (!lookup) 1 := 1)

      fun update from to =
        if from >= to then ()
        else (ArrayFn.nth (!lookup) from := !(ArrayFn.nth (!lookup) (from-1)) + !(ArrayFn.nth (!lookup) (from-2));
              update (from+1) to)

      fun memoFun x =
        if x < ArrayFn.len (!lookup) then !(ArrayFn.nth (!lookup) x)
        else
          let
            val oldLen = ArrayFn.len (!lookup)
          in
            (
            lookup := (ArrayFn.doubleLen ~1 (!lookup));
            update oldLen (oldLen * 2);
            memoFun x
            )
          end
    in
      memoFun
    end

  (* Tests *)


end
