signature PATH =
  sig
    type path
    datatype arc = Up | Down of string

    exception InvalidPath

    val root : path
    val push : arc * path -> path
  end
