signature LAZYTREE =
sig
  type 'a lazyTree (* abstract *)
  datatype 'a treeFront = LEAF | NODE of 'a lazyTree * 'a * 'a lazyTree

  (* lazy lazytree construction and exposure *)
  val delay : (unit -> 'a treeFront) -> 'a lazyTree
  val expose : 'a lazyTree -> 'a treeFront

  val map : ('a -> 'b) -> 'a lazyTree -> 'b lazyTree

  val preOrd : 'a lazyTree -> 'a Stream.stream
end

