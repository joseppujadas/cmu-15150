functor MkSolver(Puzzle : SEARCHABLE) : SOLVER =
struct
    structure Puzzle = Puzzle

    (* findOne : Puzzle.state -> (Puzzle.final -> 'a) -> (unit -> 'a) -> 'a
     * REQUIRES: true
     * ENSURES: findOne start sc fc => sc f, if the first solution state reached has outcome YES f
     *                                 fc (), if no such solution is found
     *)
    fun findOne (cur : Puzzle.state) (sc : Puzzle.final -> 'a) (fc : unit -> 'a) : 'a =
      Puzzle.getOutcome cur (fn Puzzle.YES(x) => sc x | Puzzle.NO => fc () | Puzzle.MAYBE(res) => findOne' res sc fc)

    and findOne' []      sc fc = fc ()
      | findOne' (x::xs) sc fc = findOne x sc (fn () => findOne' xs sc fc)

  (* findAll : Puzzle.state -> (Puzzle.final * (unit -> 'a) -> 'a) -> (unit -> 'a) -> 'a
   * REQUIRES: true
   * ENSURES: findAll start sc fc ~= foldr g (fc ()) L, if sc (sol, fc) ~= g(sol, fc ()), where L
   * contains all solutions
   *)
  fun findAll (cur : Puzzle.state) (sc : Puzzle.final * (unit -> 'a) -> 'a) (fc : unit -> 'a) : 'a =
    Puzzle.getOutcome cur (fn Puzzle.YES(x) => sc (x,fc) | Puzzle.NO => fc () | Puzzle.MAYBE(res) => findAll' res sc fc)

  and findAll' []      sc fc = fc ()
    | findAll' (x::xs) sc fc = findAll x sc (fn () => findAll' xs sc fc)

end
