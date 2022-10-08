signature TWIG =
sig
  (* abstract type! *)
  type 'a twig

  (* returns a twig with height 0 and containing no elements *)
  val empty : 'a twig

  (* combines a root value and two sub-twigs together into a larger twig *)
  val combine : 'a twig * 'a * 'a twig -> 'a twig

  (* returns height of twig *)
  val height : 'a twig -> int

end
