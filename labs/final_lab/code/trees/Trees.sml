structure Trees :> TREES =
struct
  datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree

  fun depth Empty = 0
    | depth (Node (L, _, R)) = 1 + Int.max (depth L, depth R)

  fun weight Empty = 0
    | weight (T as Node(L,x,R)) =
        depth T * (x + weight L + weight R)

  fun weightSum Empty = 0
    | weightSum (T as Node(L,x,R)) =
      weightSum L + weight T + weightSum R


  val T = Node(Node(Node(Empty,4,Empty),6,Node(Empty,3,Empty)),7,Node(Node(Empty,2,Empty),5,Node(Empty,1,Empty)))
  val () = print (Int.toString (weightSum T))


  fun weightSumCPS Empty k = k 0
    | weightSumCPS (T as Node(L,x,R)) k =
      weightSumCPS L (fn res => weightSumCPS R (fn res2 => k(weight T + res + res2)))

  val () = print (Int.toString (weightSumCPS T (fn x => x)))

  fun genWeight f Empty = 0
    | genWeight f (T as Node(L,x,R)) =
      depth T * f (x, f(genWeight f L, genWeight f R))

  fun genWeightSum f Empty = 0
   |  genWeightSum f (T as Node(L,x,R)) =
        genWeightSum f L + genWeight f T + genWeightSum f R

  val 199 = genWeightSum op+ T

  (* For testing *)
  val G =
    Node(Node(Node(Empty,4,Empty),6,Node(Empty,3,Empty)),7,
    Node(Node(Empty,2,Empty),5,Node(Empty,1,Empty)))

end
