(* Write a negation functor called MkNemesis here! *)
functor MkNemesis (Stu : STUDENT) :> STUDENT =
struct
  type thoughts = Stu.thoughts

  datatype status
    = YES
    | NO
    | MAYBE of thoughts * Book.direction




  val start = Stu.start

  (*fun think : thoughts * (bool * Book.page option) -> status*)
  fun think (t,c) = case Stu.think (t,c) of
    Stu.YES => NO
  | Stu.NO => YES
  | Stu.MAYBE(x,y) => MAYBE(x,y)
end
