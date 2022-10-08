signature SOLVER =
sig
    structure Puzzle : SEARCHABLE
    val findOne : Puzzle.state -> (Puzzle.final -> 'a) -> (unit -> 'a) -> 'a
    val findAll : Puzzle.state -> (Puzzle.final * (unit -> 'a) -> 'a) -> (unit -> 'a) -> 'a
end