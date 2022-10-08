structure SubsetSum : SEARCHABLE =
struct
    type params = int * int list
    type state = int list * int * int list
    type final = int list
    datatype outcome = YES of final | NO | MAYBE of state list

    (* init : params -> state
     * REQUIRES: true
     * ENSURES: init pars ~= your initial state for SubsetSum, given pars as parameters
     *)
    fun init(sum, S) = (S,sum,[])

    (* getOutcome : state -> (outcome -> 'a) -> 'a
     * REQUIRES: true
     * ENSURES: getOutcome cur k ~= k oc where oc is the outcome corresponding to cur
     *)
    fun getOutcome (_,0,sol) k = k (YES sol)
      | getOutcome ([],_,_) k = k NO
      | getOutcome (x::xs, n, sol) k = k (MAYBE ([ (xs,n-x,x::sol), (xs,n,sol) ]))

    (* eq : final * final -> bool
     * REQUIRES: true
     * ENSURES: eq(f1, f2) ~= true, if f1 and f2 represent equivalent lists
     *                        false, otherwise
     *)
    fun eq ([],[]) = true
      | eq (_,[]) = false
      | eq ([],_) = false
      | eq (x::xs,y::ys) = (x = y andalso eq (xs,ys))
      
    fun listToString L = "[" ^ (String.concatWith ", " (List.map Int.toString L)) ^ "]"
    fun stateToString (subset, sum, S) = "(" ^ (String.concatWith ", " [listToString subset, Int.toString sum, listToString S]) ^ ")"
    val finalToString = listToString
end
