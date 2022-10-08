structure Pie : PIE2 =
  struct
    exception AlreadyEmpty
    type 'a t = 'a list  (* list of slices *)
    val mkPie = fn
      nil => raise AlreadyEmpty
    | L   => L

    val eat = fn
      nil => NONE
    | _ :: slices => SOME slices
  end
