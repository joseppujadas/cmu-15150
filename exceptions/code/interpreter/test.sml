use "utils.sml";

local
  structure Utils =
    Utils (
      datatype aexp = datatype aexp
      datatype bexp = datatype bexp
      datatype command = datatype command
      type program = program
      type environment = environment
      val runCommand = runCommand
      val runCommands = runCommands
      val runProgram = runProgram
    )
in
  open Utils
  val testAll = fn () => ignore (testAll "tests")
end
