structure ArrayFn :> ARRAYFN where type 'a t = int * (int -> 'a ref) =
struct
  (* Don't change this! *)
  type 'a t = int * (int -> 'a ref)

  (* new: 'a -> int -> 'a t
   * REQUIRES: n >= 0
   * ENSURES: new x n = A where
   *     - The array has length n
   *     - Each index contains x
   *     - Ref cells at distinct indices are distinct
   *     - new x n has O(n) work and O(log n) span
   *)
  fun new x n =
  let
    fun new' i =
      let
        val payload = ref x
      in
        if i > (n div 2) then (fn y => if y = i andalso y < n then payload else raise Subscript) else (*"leaf" nodes, no children*)
          let
            val (lFun,rFun) = (new' ((2 * i) + 1), new' ((2 * i) + 2))                                
          in
            fn y => if y = i then payload else ((lFun y) handle Subscript => rFun y)
          end
      end
  in
    (n,new' 0)
  end

  (* toList: 'a t -> 'a list
   * You may optionally define this function for debugging purposes
   *)
  fun toList _ = raise Fail "Unimplemented"

  (* len: 'a t -> int
   * REQUIRES: true
   * ENSURES: len A = the length of A
   *          len A has O(1) work and span
   *)
  fun len (l,_) = l

  (* nth: 'a t -> int -> 'a ref
   * REQUIRES: true
   * ENSURES:
   *     - If i >= 0 and i < len A, then nth A i = a ref cell representing
   *       the element at index i of array A, else nth A i = raise Subscript
   *     - The ref cell is the same across multiple calls to nth
   *     - nth A i has O(log(len A)) work and span
   *)
  fun nth (_,f) x = f x

  (* doubleLen: 'a -> 'a t -> 'a t
   * REQUIRES: true
   * ENSURES: doubleLen x A = B where
   *     - The length of the new array is double the length of the old array
   *     - The first len A elements of the new array are the same as in A
   *     - The last len A elements of the new array contain x
   *     - doubleLen x A has O(len A) work and O(log(len A)) span
   *     - nth and len still meet their respective cost bounds when called
   *       on B
   *)
  fun doubleLen x (l,f) =
    let
      val (_,g) = (new x l)
      fun newFun i = if i < l then f i else g (i-l)
    in
      (l*2,newFun)
    end
end
