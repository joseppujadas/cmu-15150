structure LQ : QUEUE =
struct
     type 'a queue = 'a list

     val emp = []

     fun ins (n,l) = l @ [n]

     fun rem [] = NONE
       | rem (y::ys) = SOME (y, ys)
end

structure LLQ : QUEUE =
struct
     type 'a queue = ('a list) * ('a list)

     val emp = ([], [])

     fun ins (n, (front, back)) = (front, n::back)

     fun rem (  [] ,  [] ) = NONE
       | rem (y::ys, back) = SOME (y, (ys, back))
       | rem (  [] , back) = rem (List.rev back, [])
end
