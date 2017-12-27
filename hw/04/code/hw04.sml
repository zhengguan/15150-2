(* Defining Trees *)
datatype tree =
    Node of tree * int * tree
  | Empty


(* TASK 2 *)

(* part : int * int list -> int list * int list
 * REQUIRES: true
 * ENSURES: part(p, L) => a pair of lists (A, B) such that
 * A consists of the items in L that are less than p and
 * B consists of the items in L that are greater than or equal to p
 *)
fun part (p : int, L : int list) : int list * int list =
    case L of
      [] => ([], [])
     |x::xs => (let
                  val (A, B) = part (p, xs)
                in
                  (case x < p of
                     true => (x::A, B)
                    |false => (A, x::B))
                end)

val ([], []) = part (42, [])
val ([1, 2, 3], [6, 7, 8]) = part (5, [1, 2, 3, 6, 7, 8])
val ([~1, ~2, ~42], [0, 1, 2]) = part (0, [0, ~1, 1, 2, ~2, ~42])

(* quicksort : int list -> int list
 * REQUIRES: true
 * ENSURES: quicksort L is sorted
 *)
fun quicksort (L : int list) : int list =
    case L of
      [] => []
     |x::xs => (let
                   val (A, B) = part( x, xs)
                in
                  (quicksort A)@(x :: quicksort B)
                end)

(* testing function taken from hw04-handout.pdf*)
(* sorted : int list -> bool
 * REQUIRES: true
 * ENSURES: sorted L ==> true if L is <-sorted
 *                   ==> false otherwise
 *)
fun sorted ([] : int list) : bool = true
  | sorted ([x] : int list) : bool = true
  | sorted (x::(y::L) : int list) : bool =
      (Int.compare(x,y) <> GREATER) andalso sorted(y::L)

val true = sorted(quicksort([]))
val true = sorted(quicksort [54, 7, ~3, 0])

(* TASK 3 *)

datatype paren = LPAR | RPAR
(* string_to_parens: string -> paren list
 * REQUIRES: true
 * ENSURES: (string_to_parens s) returns the list of parentheses in s. *)
fun string_to_parens (s : string) : paren list =
    let
      fun res (L : char list) : paren list =
          case L of
            [] => []
           | #"("::xs => LPAR :: (res xs)
           | #")"::xs => RPAR :: (res xs)
           | _::xs => res xs
    in
        res (String.explode s)
    end

(* pmatch_help: (paren list * int) -> bool
 * REQUIRES: n >= 0
 * ENSURES:  pmatch_help (L, n)  returns true L is balanced and
 *           false otherwise.
 *)
fun pmatch_help ((L, n) : paren list * int) : bool =
    case L of
      [] => (n = 0)
     |x::xs => (case (n < 0) of
                  true => false
                 |false => (case x of
                              LPAR => pmatch_help(xs, n+1)
                             |RPAR => pmatch_help(xs, n-1)))

val true = pmatch_help ([], 0)
val true = pmatch_help ([LPAR, LPAR, RPAR, LPAR, RPAR, RPAR], 0)
val false = pmatch_help ([RPAR, LPAR, RPAR,LPAR], 0)
val true = pmatch_help ([RPAR, LPAR, RPAR, LPAR, RPAR], 1)

(* pmatch : string -> bool
 * REQUIRES: s is a string consisting solely of either "(" or ")" concatenated
 * together
 * ENSURES: pmatch s => true if s is a well matched string of parentheses, and
 * false otherwise
 *)
fun pmatch (s : string) : bool =
    pmatch_help(string_to_parens(s), 0)

val true = pmatch ""
val true = pmatch "()"
val true = pmatch "((hello))(there)"
val false = pmatch "())"
val false = pmatch "((()"

(* TASK 5 *)

(* countNonZero : tree -> int
 * REQUIRES: true
 * ENSURES: countNonZero t => n, where n = the number of non-zero values in t
 *)
fun countNonZero (t : tree) : int =
    case t of
      Empty => 0
     |Node(l, x, r) => (case x = 0 of
                          true => countNonZero(l) + countNonZero(r)
                         |false => 1 + countNonZero(l) + countNonZero(r))

val tree1 = Node(Node(Node(Empty,0,Empty),0,Node(Empty,0,Empty)),0,Node(Node(Empty,0,Empty),4,Node(Empty,7,Empty)))
val tree2 = Node(Node(Empty,0,Empty),4,Node(Empty,7,Empty))
val tree3 = Node(Node(Empty,13,Empty),0,Node(Empty,~13,Empty))
val 0 = countNonZero Empty
val 0 = countNonZero (Node(Node(Empty,0,Node(Empty,0,Empty)),0,Node(Empty,0,Empty)))
val 1 = countNonZero (Node(Node(Empty,5,Node(Empty,0,Empty)),0,Node(Empty,0,Empty)))
val 4 = countNonZero (Node(Node(Empty,5,Node(Empty,~1,Empty)),42,Node(Empty,69,Empty)))
val 2 = countNonZero tree1
val 2 = countNonZero tree2
val 2 = countNonZero tree3

(* treeEq : tree * tree -> bool, taken from lab04.sml
 * REQUIRES: true
 * ENSURES: treeEq(t1, t2) => true if t1 = t2, and false otherwise
 *)
fun treeEq (t1 : tree, t2 : tree) : bool =
    case (t1, t2) of
      (Empty, Empty) => true
     |(Node(l1, x1, r1), Node(l2, x2, r2)) => ((x1 = x2) andalso
                                               treeEq(l1, l2) andalso
                                               treeEq(r1, r2))
     |_ => false

val true = treeEq(Empty, Empty)
val true = treeEq(tree1, tree1)
val true = treeEq(tree2, tree2)

(* lca : tree -> tree
 * REQUIRES: t is a binary tree with two nodes that have non-zero values,
 * and all other nodes with value 0
 * ENSURES: lca t => t', the lowest common ancestor of the two non-zero nodes
 *)
fun lca (t : tree) : tree =
    case t of
     Node(l, x, r) =>
      (case (countNonZero(l), countNonZero(r)) of
         (0,2) => lca(r)
        |(2,0) => lca(l)
        |(1,_) => Node(l, x, r)
        |(_,1) => Node(l, x, r)
        |(_,_) => raise Fail "This is not a correct tree!")
    |Empty => raise Fail "This is not a correct tree!"

val true = treeEq(tree3,lca(tree3))
val true = treeEq(tree2, lca(tree2))
val true = treeEq(tree2, lca(tree1))

(* depth : tree -> int
 * REQUIRES: t is a non-zero tree with only one non-zero element
 * ENSURES: depth t => the depth of the node with the non-zero element
 *)
fun depth (t : tree) : int =
    case t of
      Empty => raise Fail "This is not a correct input!"
     |Node (l, x, r) =>
      (case (x = 0) of
         false => 0
        |true =>
         (case countNonZero(l) of
            1 => 1 + depth(l)
           |0 => 1 + depth(r)
           |_ => raise Fail "This is not a correct input!"))

val 0 = depth(Node(Empty, 5, Empty))
val 2 = depth(Node(Node(Node(Empty,5,Empty),0,Empty),0,Node(Empty,0,Empty)))

(* distance : tree -> int
 * REQUIRES: t is a tree with two non-zero elements at nodes n1 and n2, and
 * all other elements equal to zero.
 * ENSURES: distance t => the distance between n1 and n2
 *)
fun distance (t : tree) : int =
    case t of
      Empty => raise Fail "This is not a correct input!"
     |_ => (let
              val Node(l, x, r) = lca(t)
            in
              (case ((x = 0), countNonZero(l)) of
                 (true,_) => (1 + depth(l)) + (1 + depth(r))
               |(false, 0) => 1 + depth(r)
               |(false, 1) => 1 + depth(l)
               |(_,_) => raise Fail "This is not a correct input!")
            end)

val 1 = distance(tree2)
val 2 = distance(tree3)
val tree4 = Node(Node(Node(Node(Empty,0,Empty),0,Node(Empty,0,Empty)),0,Node(Node(Empty,0,Empty),0,Node(Empty,0,Empty))),0,Node(Node(Node(Empty,7,Empty),0,Node(Empty,0,Empty)),0,Node(Node(Empty,0,Empty),0,Node(Empty,4,Empty))))
val 4 = distance(tree4)

(* TASK 6 *)

(* treecompare : tree * tree -> order
 * REQUIRES: true
 * ENSURES: treecompare (t1, t2) => LESS | EQUAL | GREATER depending on the
 * outcome of a comparison between the values at the root nodes of t1 and t2
 *)
fun treecompare (t1 : tree, t2 : tree) : order =
    case (t1, t2) of
      (Empty, Empty) => EQUAL
     |(Empty, _) => LESS
     |(_,Empty) => GREATER
     |(Node(l1, x1, r1), Node(l2, x2, r2)) => Int.compare(x1, x2)

val EQUAL = treecompare(Empty, Empty)
val GREATER = treecompare(Node(Empty, 5, Empty), Empty)
val LESS = treecompare(Empty, Node(Empty, ~3, Empty))
val EQUAL = treecompare(tree1, tree3)
val LESS = treecompare(tree1, tree2)

(* swapDown : tree -> tree
 * REQUIRES: the subtrees of t are both maxheaps or t is empty
 * ENSURES: swapDown(t) => t if t is Empty, otherwise
                        => a maxheap containing the elements in t
 *)
fun swapDown (t : tree) : tree =
    case t of
      Empty => t
     |Node(l, x, r) => (case (l, r) of
                          (Empty, Empty) => t
                          |(Empty,Node(rl,rx,rr)) =>
                           (case treecompare(t,r) of
                              LESS => Node(l,rx,swapDown(Node(rl,x,rr)))
                             |_ => t)
                          |(Node(ll,lx,lr),Empty) =>
                           (case treecompare(t,l) of
                              LESS => Node(swapDown(Node(ll,x,lr)),lx,r)
                             |_ => t)
                          |(Node(ll,lx,lr),Node(rl,rx,rr)) =>
   (case (treecompare(t,l),treecompare(t,r)) of
        (LESS,LESS) =>
         (case (Int.compare(lx, rx)) of
          GREATER => Node(swapDown(Node(ll,x,lr)),lx,r)
         |_ => Node(l,rx,swapDown(Node(rl,x,rr))))
        |(_,LESS) => Node(l,rx,swapDown(Node(rl,x,rr)))
        |(LESS,_) => Node(swapDown(Node(ll,x,lr)),lx,r)
        |(_,_) => t))

val Empty = swapDown Empty
val Node(Empty,11,Empty) = swapDown (Node(Empty, 11, Empty))
val heap1 = Node(Node(Empty,4,Empty),11,Node(Empty,7,Empty))
val Node(Node(Empty,7,Empty),11,Node(Empty,4,Empty)) = swapDown(Node(Node(Empty,7,Empty),4,Node(Empty,11,Empty)))

(* heapify : tree -> tree
 * REQUIRES: true
 * ENSURES: heapify t -> t', a maxheap with exactly the elements of t
 *)
fun heapify (t : tree) : tree =
    case t of
      Empty => Empty
     |Node(l, x, r) => swapDown(Node(heapify(l),x,heapify(r)))

val Empty = heapify Empty
val Node(Empty,11,Empty) = heapify(Node(Empty,11,Empty))
val EQUAL = treecompare(heap1,heapify(Node(Node(Empty,11,Empty),4,Node(Empty,7,Empty))))
