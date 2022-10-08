signature STUDENT =
sig
  type thoughts (* abstract *)
  datatype status
    = YES
    | NO
    | MAYBE of thoughts * Book.direction

  val start : thoughts  (* the student's starting thoughts *)

  (* REQUIRES: true
   * ENSURES:
   *   Given current thoughts t and a book state p (containing
   *   information about the current page) to consider:
   *     think (t,p) = YES iff the student "likes" the book
   *     think (t,p) = NO iff the student "dislikes" the book
   *     think (t,p) = MAYBE (t',d) iff the student remains
   *       undecided, with updated thoughts t' and a direction
   *       d in which to flip to change the state of the book
   *)
  val think : thoughts * (bool * Book.page option) -> status
end
