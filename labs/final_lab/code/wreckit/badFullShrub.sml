signature NUM =
sig
  type t

  val one : t
  val is_one : t -> bool
  val div2 : t -> t
  val double : t -> t
end

structure Nums : NUM =
struct
  type t = int

  val one = 1
  fun is_one n = n = 1
  fun div2 n = n div 2
  fun double n = n + n
end

signature FULLSHRUB =
sig
  structure S : NUM

  type 'a shrub
  type 'a fullshrub = S.t * 'a shrub

  exception Wrecked

  val init : 'a -> 'a fullshrub
  val inord : 'a fullshrub -> 'a list
  val grow : 'a fullshrub -> 'a fullshrub

end

functor FullShrub (Nums : NUM) :> FULLSHRUB =
struct
  datatype 'a shrub = Leaf of 'a | Node of 'a shrub * 'a shrub
  type 'a fullshrub = Nums.t * 'a shrub
  structure S = Nums

  exception Wrecked

  fun init x = (S.one, Leaf x)

  fun inord (s, t) =
      case (S.is_one s, t) of
        (true, Leaf x) => [x]
      | (false, Node (L, R)) =>
          inord (S.div2 s, L) @ inord (S.div2 s, R)
      | _ => raise Wrecked

  fun grow (s, t) = (S.double s, Node (t, t))
end
