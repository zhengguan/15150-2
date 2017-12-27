(* ---------------------------------------------------------------------- *)
(* Functions provided by the course staff. *)

(* max: int * int -> int
 * REQUIRES: true
 * ENSURES: max (x, y) ==> the greater of x or y
 *)
fun max (n1 : int, n2 : int) : int =
    case n1 < n2 of
      true => n2
    | false => n1

val 4 = max (1, 4)
val 0 = max (~4, 0)
val 2 = max (2, 2)

(* split: int list -> int list * int * int list
 * REQUIRES: l is non-empty
 * ENSURES: there exist l1,x,l2 such that
 *     split l ==> (l1,x,l2) and
 *     l is l1 @ x::l2 and
 *     length(l1) and length(l2) differ by no more than 1
 *)
fun split (l : int list) : (int list * int * int list) =
    case l of
      [] => raise Fail "split should never be called on an empty list"
    | _ =>
      let
        val midlen = (length l) div 2
        val front = (List.take (l,midlen))
        (* because we round down, if the list is non-empty,
         *  this has at least one thing in it *)
        val x :: back = (List.drop (l,midlen))
      in
        (front, x, back)
      end


(* ---------------------------------------------------------------------- *)
(* Functions you, the student, need to implement. *)

(***** Section 2: Depth  *****)

datatype tree =
    Empty
  | Node of (tree * int * tree)

(* Task 2.1 *)

(* work: tree -> int
 * REQUIRES: true
 * ENSURES: work T computes the total work of a computation tree T
 *)
fun work (Empty : tree) : int = 0
  | work (Node (l, x, r)) = x + (work l) + (work r)

val 0 = work Empty
val testT1 = Node(Node(Node(Empty, 1, Empty), 2, Node(Empty, 0, Empty)), 3,
                 Node(Empty, 4, Empty))
val 10 = work testT1
val testT2 = Node(Node(Empty, 2, Empty), 3,
                  Node(Node(Empty, 1, Empty), 5, Empty))
val 11 = work testT2

(* span: tree -> int
 * REQUIRES: true
 * ENSURES: span T computes the span (the path with the greatest work)
 *          of a computation tree T
 *)
fun span (Empty : tree) : int = 0
  | span (Node (l, x, r)) = x + max(span l, span r)

val 0 = span Empty
val 7 = span testT1
val 9 = span testT2

(* ---------------------------------------------------------------------- *)

(***** Section 3: Lists to Trees *****)

(* Task 3.1 *)
(* listToTree: int list -> tree
 * REQUIRES: true
 * ENSURES: listToTree l returns a balanced tree transformed from l
 *)
fun listToTree ([] : int list) : tree = Empty
  | listToTree (l) =
    let
      val (l1, x, l2) = split l
    in
      Node (listToTree l1, x , listToTree l2)
    end

val Empty = listToTree nil
val Node (Empty , 3 , Empty) = listToTree [3]
val Node(Node(Empty,5,Empty),8,Node(Empty,2,Empty)) = listToTree [5,8,2]

(* treeToList: tree -> int list
 * REQUIRES: true
 * ENSURES: treeToList t returns a list obtained from
 *          the inorder traversal of t
 *)
fun treeToList (t : tree) : int list =
    case t of
      Empty => []
    | Node (l,x,r) => treeToList l @ (x :: (treeToList r))


(* ---------------------------------------------------------------------- *)

(***** Section 4: Reverse *****)


(* Task 4.1 *)

(* revT: tree -> tree
 * REQUIRES: true
 * ENSURES: revT t returns the "mirror image" of t
 *)
fun revT (Empty : tree) : tree = Empty
  | revT (Node(t1,x,t2)) = Node (revT t2, x , revT t1)

val t1 = (Node(Node(Empty,15,Empty),8,Node(Empty,4,Empty)))
val [4,8,15] = treeToList (revT t1)
val [4,8,15] = rev (treeToList t1)
val Empty = revT Empty
val Node(Empty,1,Empty) = revT (Node(Empty,1,Empty))
val Node(Node(Empty,4,Empty),8,Node(Empty,15,Empty)) = revT t1

(* ---------------------------------------------------------------------- *)

(***** Section 4: Binary Search *****)

(* Task 5.1 *)

(* binarySearch: tree * int -> bool
 * REQUIRES: t is sorted
 * ENSURES: binarySearch (t,x) returns true if x is in t, false otherwise
 *          Work and span should be O(depth of t)
 *)
fun binarySearch (Empty : tree, _ : int) : bool = false
  | binarySearch (Node(l,y,r), x) =
    (case Int.compare (x, y) of
       EQUAL => true
     | LESS => binarySearch (l, x)
     | GREATER => binarySearch (r, x))

val t2 = Node (Node (Empty, 5, Empty), 6, Empty)
val t3 = Node (Node (Empty, 1, Empty), 2, Node (Empty, 3, Empty))

val true = binarySearch (t3, 3)
val false = binarySearch (t2, 3)
val false = binarySearch (Node (t3, 4, t2), 7)
val true = binarySearch (Node (t3, 4, t2), 6)

