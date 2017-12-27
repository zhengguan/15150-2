structure Combinations =
struct
  datatype decision = Add of int
                     |Subtract of int
                     |Mult of int
                     |Div of int

  type stree = int list

  type elem = int

  type result = decision list

  (* branch : stree -> (decision * stree) list
   * REQUIRES: st is a valid stree, i.e. a non-empty int list
   * ENSURES: branch st ==> a list of decision and sub-stree pairs
   *          that describe the possible branches one can take
   *          from the root (current node) of st
   *)
  fun branch (st : stree) : (decision * stree) list =
      case st of
        [] => []
       |[x] => []
       |x::y::xs => (case y of
                       0 => [(Add y, x::xs),(Subtract y, x::xs),
                             (Mult y, 0::xs)]
                      |_ => [(Add y, (x+y)::xs),(Subtract y, (x-y)::xs),
                             (Mult y, (x*y)::xs),(Div y, (x div y)::xs)])

  (* atRoot : (elem * stree) -> bool
   * REQUIRES: st is a valid stree, i.e. a non-empty int list
   * ENSURES: atRoot (e,st) ==> true if the element at the root of
   *          the search tree is the element we're looking for, and
   *                        ==> false otherwise
   *)
  fun atRoot (e : elem, st : stree) : bool =
      case st of
        [] => false
       |x::xs => (x = e)

  val [] = branch []
  val [] = branch [4]
  val [(Add 5, [9,6,7]),(Subtract 5, [~1,6,7]),
       (Mult 5, [20,6,7]),(Div 5, [0,6,7])] = branch [4,5,6,7]
  val [(Add 0, [6,9]), (Subtract 0, [6,9]),
       (Mult 0, [0,9])] = branch [6,0,9]
  val [(Add ~1, [0,0]), (Subtract ~1, [2,0]),
       (Mult ~1, [~1,0]), (Div ~1, [~1,0])] = branch [1,~1,0]
  val true = atRoot(5,[5,6,7,8])
  val false = atRoot(5,[6,7,8,9])
  val false = atRoot(5,[])
end
