signature BOOK =
sig
  type page = string (* concrete *)
  datatype direction = FORWARD | BACKWARD
  type book (* abstract *)

  exception OutOfBounds

  (* REQUIRES: true
   * ENSURES: bind l = b, such that
   *   - b encodes all pages in l
   *   - b is opened to the first page in l, if one exists
   *   - b preserves the order of the pages in l
   *)
  val bind : page list -> book

  (* REQUIRES: true
   * ENSURES:
   *   - unbind b = l, where l consists of all pages in b
   *   - unbind (bind l) = l
   *)
  val unbind : book -> page list

  (* REQUIRES: true
   * ENSURES:
   *   - currentPage b = (true,  NONE) iff b is empty
   *   - currentPage b = (true,  SOME p) iff b is opened to page p
   *       and p is the first page in b
   *   - currentPage b = (false, NONE) iff b is non-empty
   *       and encodes the end of the book
   *   - currentPage b = (false,  SOME p) iff b is opened to page p
   *       and p is not the first page in b
   *)
  val currentPage : book -> bool * page option

  (* REQUIRES: true
   * ENSURES:
   *   flip FORWARD b raises OutOfBounds iff
   *     currentPage b encodes the end of b
   *   flip FORWARD b = b' otherwise,
   *     where currentPage b' encodes the next page in b
   *   flip BACKWARD b raises OutOfBounds iff
   *     currentPage b encodes the beginning of b
   *   flip BACKWARD b = b' otherwise,
   *     where currentPage b' encodes the previous page in b
   *)
  val flip : direction -> book -> book
end
