structure Shrub =
struct
  datatype 'a shrub = Empty | Leaf of 'a
                    | Node of 'a shrub * 'a shrub * 'a shrub

  datatype decision = Left | Center | Right

  type elem = int

  type stree = elem shrub

  type result = decision list

  (* branch : stree -> (decision * stree) list
   * REQUIRES: true
   * ENSURES: branch st ==> a list of decisions and sub-strees
   *          that one can make from the root (current node) of the
   *          search tree
   *)
  fun branch (st : stree) : (decision * stree) list =
      case st of
        Node(l,c,r) => [(Left,l),(Center,c),(Right,r)]
       |_ => []

  (* branch : (elem * stree) -> bool
   * REQUIRES: e is valuable
   * ENSURES: atRoot (e,st) ==> true if we have found the element at
   *          the root of the stree, and
   *                        ==> false otherwise
   *)
  fun atRoot (e : elem, st : stree) : bool  =
      case st of
        Leaf x => (e = x)
       |_ => false

  val [] = branch Empty
  val [(Left, Leaf 38),
       (Center, Leaf 75),
       (Right, Leaf 89)] = branch (Node(Leaf 38, Leaf 75, Leaf 89))
  val [(Left,Node(Leaf 1, Leaf 2, Leaf3)),
       (Center,Node(Leaf 4, Leaf 5, Leaf 6)),
       (Right, Node(Leaf 7, Leaf 8, Leaf 9))] =
       branch (Node(Node(Leaf 1, Leaf 2, Leaf 3),
                   Node(Leaf 4, Leaf 5, Leaf 6),
                   Node(Leaf 7, Leaf 8, Leaf 9)))
  val false = atRoot(75, (Node(Leaf 38, Leaf 75, Leaf 89)))
  val false = atRoot(75, (Leaf 76))
  val true = atRoot(75, (Leaf 75))
end
