functor MkNSearch(structure Puzzle : SEARCHABLE
                  val count : int) : SEARCHABLE =
struct
    type params = Puzzle.params
    type state = Puzzle.state list * P list
    type final = Puzzle.final list
    datatype outcome = YES of final | NO | MAYBE of state list

    (* init : params -> state
     * REQUIRES: true
     * ENSURES: init pars ~= your initial state for the NSolver, given pars as parameters
     *)
    fun init pars = raise Fail "unimplemented"

    (* getOutcome : state -> (outcome -> 'a) -> 'a
     * REQUIRES: true
     * ENSURES: getOutcome cur k ~= k oc where oc is the outcome corresponding to cur
     *)
    fun getOutcome state k = raise Fail "unimplemented"

    (* eq : final * final -> bool
     * REQUIRES: true
     * ENSURES: eq(f1, f2) ~= true, if f1 and f2 represent equivalent solutions
     *                        false, otherwise
     *)
    val eq = fn _ => raise Fail "unimplemented"

    fun stateToString (F, I) =
        "Finished:\n" ^ (String.concatWith "\n" (List.map Puzzle.finalToString F))
        ^
        "\nIn-progress:\n" ^ (String.concatWith "\n" (List.map Puzzle.stateToString I))

    fun finalToString F = String.concatWith "\n" (List.map Puzzle.finalToString F)

end
