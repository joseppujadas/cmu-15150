signature QUEUE =
sig
  type 'a queue (* abstract *)
  val emp : 'a queue
  val ins : 'a * 'a queue -> 'a queue
  val rem : 'a queue -> ('a * 'a queue) option
end
