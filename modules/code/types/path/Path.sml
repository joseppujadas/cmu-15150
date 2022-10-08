structure Path :> PATH =
  struct
    type path = string list
    datatype arc = Up | Down of string

    exception InvalidPath

    val root = nil
    val push = fn
      (Up    , nil  ) => raise InvalidPath
    | (Up    , p::ps) => ps
    | (Down p, ps   ) => p::ps

  end

val "modules" :: _ = List.foldl Path.push Path.root [
  Path.Down "private",
  Path.Down "15150",
  Path.Down "modules"
]
