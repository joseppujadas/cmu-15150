datatype tree = Empty | Node of tree * int * tree

(* isSymmetric : tree -> bool
 * REQUIRES: true
 * ENSURES: isSymmetric T ==>* true iff T is symmetric w.r.t the root
 *)
fun isSymmetric (Empty : tree) : bool = true
  | isSymmetric (Node (L,x,R) : tree) =
  case (L,R) of
    (Empty,Empty) => true
  | (Node(LL,xL,LR), Node(RL,xR,RR)) => isSymmetric(L) andalso isSymmetric(R)
  | (Node(LL,xL,LR), Empty) => false
  | (Empty, Node(RL,xR,RR)) => false

val true = isSymmetric(Node(Empty , 1, Empty))
val true = isSymmetric(Node(Node(Node(Node(Node(Empty,23,Empty),16,Node(Empty,42,Empty)),15,Node(Node(Empty,3,Empty),3,Node(Empty,3,Empty))),8,Node(Node(Node(Empty,3,Empty),3,Node(Empty,3,Empty)),2,Node(Node(Empty,3,Empty),3,Node(Empty,3,Empty)))),4,Node(Node(Node(Node(Empty,2,Empty),2,Node(Empty,1,Empty)),1,Node(Node(Empty,1,Empty),1,Node(Empty,1,Empty))),1,Node(Node(Node(Empty,1,Empty),1,Node(Empty,1,Empty)),1,Node(Node(Empty,1,Empty),1,Node(Empty,1,Empty))))))
val false = isSymmetric(Node(Node(Node(Node(Node(Empty,23,Empty),16,Node(Empty,42,Empty)),15,Node(Node(Empty,3,Empty),3,Node(Empty,3,Empty))),8,Node(Node(Node(Empty,3,Empty),3,Node(Empty,3,Empty)),2,Node(Node(Empty,3,Empty),3,Node(Empty,3,Empty)))),4,Node(Node(Node(Empty,2,Node(Empty,1,Empty)),1,Node(Node(Empty,1,Empty),1,Node(Empty,1,Empty))),1,Node(Node(Node(Empty,1,Empty),1,Node(Empty,1,Empty)),1,Node(Node(Empty,1,Empty),1,Node(Empty,1,Empty))))))
val false = isSymmetric(Node(Node(Node(Node(Node(Empty,23,Empty),16,Node(Empty,42,Empty)),15,Node(Node(Empty,3,Empty),3,Node(Empty,3,Empty))),8,Empty),4,Node(Node(Node(Empty,2,Node(Empty,1,Empty)),1,Node(Node(Empty,1,Empty),1,Node(Empty,1,Empty))),1,Node(Node(Node(Empty,1,Empty),1,Node(Empty,1,Empty)),1,Node(Node(Empty,1,Empty),1,Node(Empty,1,Empty))))))
