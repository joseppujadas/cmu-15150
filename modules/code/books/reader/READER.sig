signature READER =
sig
  structure Student : STUDENT

  (* REQUIRES: true
   * ENSURES:
   *   read b = true iff Student's status is YES
   *     upon thinking repeatedly about the pages of b
   *   read b = false iff Student's status is NO
   *     upon thinking repeatedly about the pages of b
   *)
  val read : Book.book -> bool
end
