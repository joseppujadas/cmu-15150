signature SEARCHABLE =
sig
    type params
    type state
    type final
    datatype outcome = YES of final | NO | MAYBE of state list
    val init : params -> state
    val getOutcome : state -> (outcome -> 'a) -> 'a
    val eq : final * final -> bool
    val stateToString : state -> string
    val finalToString : final -> string
end