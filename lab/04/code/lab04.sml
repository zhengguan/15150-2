(* ---------------------------------------------------------------------- *)
(* Functions provided by the course staff. *)

(* max : int * int -> int
 * REQUIRES: true
 * ENSURES: max (x, y) ==> the greater of x or y
 *
 * Examples:
 *  max (1, 4) ==> 4
 *  max (~4, 0) ==> 0
 *  max (2, 2) ==> 2
 *)
fun max (n1 : int, n2 : int) : int =
    case Int.compare(n1,n2) of
         LESS => n2
       | _ => n1

val 4 = max (1, 4)
val 0 = max (~4, 0)
val 2 = max (2, 2)

(* split : int list -> int list * int * int list
 * REQUIRES: l is non-empty
 * ENSURES: there exist l1,x,l2 such that
 *    split l == (l1,x,l2) and
 *    l == l1 @ x::l2 and
 *    length(l1) and length(l2) differ by no more than 1
 *)
fun split ([] : int list) : (int list * int * int list) =
    raise Fail "split should never be called on an empty list"
  | split l =
    let
      val midlen = (length l) div 2
      val front = (List.take (l,midlen))

      (* because we round down, if the list is non-empty, this
       * has at least one thing in it
       *)
      val x::back = (List.drop (l,midlen))
    in
      (front, x, back)
    end


(* ---------------------------------------------------------------------- *)
(* Functions you, the student, need to implement. *)

(***** Section 2: Depth  *****)

datatype tree =
    Empty
  | Node of (tree * int * tree)

(* treeEq : tree * tree -> bool
 * REQUIRES: true
 * ENSURES: returns true if the given trees are equal and false otherwise
 *)
fun treeEq (Empty, Empty) = true
  | treeEq (Node(l1, x1, r1), Node(l2, x2, r2)) =
      (x1 = x2) andalso treeEq(l1, l2) andalso treeEq(r1, r2)
  | treeEq _ = false

(* Task 2.1 *)
(* work : tree -> int
 * REQUIRES: t is a computation tree
 * ENSURES: work t => the total work of the computation tree
 *)
fun work (t : tree) : int =
    case t of
      Empty => 0
     |Node(l, x, r) => 1 + (work l) + (work r)

val 0 = work Empty
val 3 = work (Node(Empty,5,Node(Empty,3,Node(Empty,2,Empty))))

(* Task 2.2 *)
(* span : tree -> int
 * REQUIRES: t is a computation tree
 * ENSURES: span t => span of the computation tree t
 *)
fun span (t : tree) : int =
    case t of
      Empty => 0
     |Node(l, x, r) => 1 + max((span l),(span r))

(* ---------------------------------------------------------------------- *)

(***** Section 3: Lists to Trees *****)

(* Task 3.1 *)

fun listToTree (l : int list) : tree =
    case l of
      [] => Empty
     |x::xs => (let
                 val (l, y, r) = split (x::xs)
               in
                 Node(listToTree l, y, listToTree r)
               end)

val Empty = listToTree []
val Node(Node(Node(Empty,~2,Empty),42,Empty),5,Node(Node(Empty,7,Empty),~13,Empty)) = listToTree [~2,42,5,7,~13]

(* ---------------------------------------------------------------------- *)

(***** Section 4: Reverse *****)

(* reverse : list -> list
 * REQUIRES: true
 * ENSURES: reverse L => reverse order permutation of L
 *)
fun reverse (L : 'a list) : 'a list =
    case L of
      [] => []
     |x::xs => reverse(xs) @ [x]

val [] = reverse []
val [5, 4, 3, 2, 1] = reverse [1, 2, 3, 4, 5]

(* treeToList : tree -> int list
 * REQUIRES: true
 * ENSURES: returns a list of the elements in the tree,
 *           ordered by an in-order traversal
 *)
fun treeToList (Empty : tree) : int list = []
  | treeToList (Node(l,x,r)) = treeToList l @ (x :: (treeToList r))

(* Task 4.1 *)
fun revT (t : tree) : tree =
    case t of
      Empty => Empty
     |Node(l, x, r) => Node(r, x, l)

val tree1 = Node (Node(Empty,5,Empty), 13, Node(Empty,8,Empty))
val true = (treeToList(revT tree1) = reverse(treeToList tree1))

(* ---------------------------------------------------------------------- *)

(***** Section 5: Binary Search *****)

(* Task 5.1 *)

(* binarySearch : tree * int -> bool
 * REQUIRES: t : tree is sorted
 * ENSURES: binarySearch (t, x) => true if x in t, false otherwise
 *)
fun binarySearch (t : tree, x : int) : bool =
    case t of
      Empty => false
     |Node (l, y, r) => (case Int.compare(x,y) of
                           GREATER => binarySearch(r, x)
                          |EQUAL => true
                          |LESS => binarySearch(l, x))
