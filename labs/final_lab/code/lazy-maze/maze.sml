datatype direction = UP | DOWN| LEFT | RIGHT
datatype maze = Maze of direction -> decision
and      decision = NO of maze | YES of maze

fun makeHorizontal () = raise Fail "unimplemented"

fun makePlus () = raise Fail "unimplemented"

fun makeNTrap M n = raise Fail "unimplemented"

fun areEqualToNSteps M1 M2 n = raise Fail "unimplemented"

fun makeSpiral () = raise Fail "unimplemented"

(* Helper functions for testing and specifications *)

fun step(LEFT, x, y) = (x - 1, y)
  | step(RIGHT, x, y) = (x + 1, y)
  | step(UP, x, y) = (x, y + 1)
  | step(DOWN, x, y) = (x, y - 1)

fun tryPath' _ [] (x, y) = (x, y)
  | tryPath' (Maze f) (d::ds) (x, y) =
      case f d of
        YES M' => tryPath' M' ds (step(d, x, y))
       | NO M' => tryPath' M' ds (x, y)

fun tryPath M L = tryPath' M L (0, 0)


fun pathLength' (Maze M) [] x = x
  | pathLength' (Maze M) (d::ds) x =
     case M d of
       YES M' => pathLength' M' ds (x + 1)
       | NO M' => pathLength' M' ds x

fun pathLength M L = pathLength' M L 0
