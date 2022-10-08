(* Implement MkCompose, taking in StudentA and StudentB, here! *)
functor MkCompose (
  structure StudentA : STUDENT
  structure StudentB : STUDENT
) :> STUDENT =
  struct

    type thoughts = StudentA.thoughts * StudentB.thoughts * bool

    datatype status
      = YES
      | NO
      | MAYBE of thoughts * Book.direction


    val start = (StudentA.start, StudentB.start, false)

    fun think ((s1,s2,s1Done),c) =
      if not s1Done then
        case StudentA.think (s1, c) of
          StudentA.NO => NO
        | StudentA.MAYBE(x,y) => MAYBE((x,s2, false), y)
        | StudentA.YES => think((s1,s2,true),c)
      else
        case StudentB.think(s2,c) of
          StudentB.NO => NO
        | StudentB.MAYBE(x,y) => MAYBE((s1,x,true), y)
        | StudentB.YES => YES

  end
