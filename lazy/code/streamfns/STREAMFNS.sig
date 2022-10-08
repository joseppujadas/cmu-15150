signature STREAMFNS =
sig
  val concat: 'a Stream.stream Stream.stream -> 'a Stream.stream
  val cycle: 'a Stream.stream -> 'a Stream.stream
  val interleave: 'a Stream.stream -> 'a Stream.stream -> 'a Stream.stream
  val double: 'a Stream.stream -> 'a Stream.stream
end
