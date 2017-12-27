(* datatype declarations *)
datatype 'a shrub = Leaf of 'a
                  | Branch of 'a shrub * 'a shrub

datatype 'a depthTree = DEmpty
                      | DNode of ('a depthTree) * ('a * int) * ('a depthTree)

datatype tower = TowerA | TowerB | TowerC
datatype disk = Disk of int

type assignment = string * int
type relationship = string * string list

(* Task 3.1 : findOne:
 * REQUIRES: p is a total function
 * ENSURES: findOne p T s k ==> s v, where v is the leftmost value in T
 * such that p v ==> true, or ==> k () if such a value does not exist
 *)
fun findOne (p : 'a -> bool) (T : 'a shrub)
            (s : 'a -> 'b) (k : unit -> 'b) : 'b =
    case T of
      Leaf v => (case (p v) of
                   true => s v
                  |false => k())
     |Branch(l,r) => findOne p l s (fn () : unit => findOne p r s k)

val "4" = findOne (fn x => x mod 2 = 0) (Branch(Branch(Leaf 5, Leaf 4),Leaf 6))
                     (fn x => Int.toString x) (fn () => "")
val "" = findOne (fn x => x mod 2 = 0) (Branch(Branch(Leaf 5, Leaf 3),Leaf 9))
                     (fn x => Int.toString x) (fn () => "")
val SOME 16 = findOne (fn x => x > 15)
                      (Branch(Branch(Leaf 8, Leaf ~1),Branch(Leaf 15,
                                                             Leaf 16)))
                      (fn x => SOME x) (fn () => NONE)
val NONE  = findOne (fn x => x > 15)
                      (Branch(Branch(Leaf 8, Leaf ~1),Branch(Leaf 15,
                                                             Leaf 15)))
                      (fn x => SOME x) (fn () => NONE)

(* Task 3.2 : findTwo: ('a -> bool) -> ('a * 'a -> bool) -> 'a shrub ->
 *                     ('a * 'a -> 'b) -> (unit -> 'b) -> 'b
 * REQUIRES: p is a total function
 * ENSURES: findTwo p eq T s k ==> s (v1, v2), where v1 and v2 are distinct
 * values in T such that (eq(v1, v2)) ==> false, p v1 ==> true and
 * p v2 ==> true, or ==> k () if no such values v1 and v2 exist
 *)
fun findTwo (p : 'a -> bool) (eq : 'a * 'a -> bool) (T : 'a shrub)
	        (s : 'a * 'a ->'b) (k : unit -> 'b) : 'b  =
    case findOne p T (fn x => SOME x) (fn () => NONE) of
      SOME x => findOne (fn y => p y andalso (not (eq (x,y)))) T
                (fn y => s (x,y)) k
     |NONE => k ()

val 5 = findTwo (fn x => x < 0) (fn (x,y) => (x = y))
                (Branch(Leaf ~1, Leaf ~5)) (fn (x,y) => x * y) (fn () => 0)
val 0 = findTwo (fn x => x < 0) (fn (x,y) => (x = y))
                (Branch(Leaf ~1, Leaf 7)) (fn (x,y) => x * y) (fn () => 0)
val 0 = findTwo (fn x => x < 0) (fn (x,y) => (x = y))
                (Branch(Leaf ~1, Leaf ~1)) (fn (x,y) => x * y) (fn () => 0)
val "10" = findTwo (fn x => x mod 2 = 0) (fn (x,y) => (x=y))
                  (Branch(Branch(Leaf 5, Leaf 4),Leaf 6))
                  (fn (x,y) => Int.toString (x+y)) (fn () => "")
val "" = findTwo (fn x => x mod 2 = 0) (fn (x,y) => (x=y))
                  (Branch(Branch(Leaf 5, Leaf 4),Leaf 4))
                  (fn (x,y) => Int.toString (x+y)) (fn () => "")
val SOME 3 = findTwo (fn x => x > 15) (fn (x,y) => (x=y))
                      (Branch(Branch(Leaf 8, Leaf 35),Branch(Leaf 15,
                                                             Leaf 16)))
                      (fn (x,y) => SOME (x mod y)) (fn () => NONE)
val NONE  = findTwo (fn x => x > 15) (fn (x,y) => (x=y))
                      (Branch(Branch(Leaf 8, Leaf 35),Branch(Leaf 15,
                                                             Leaf 35)))
                      (fn (x,y) => SOME (x mod y)) (fn () => NONE)


(* Task 4.1:hanoi*)

(* hanoi_helper : disk list * tower * tower * tower ->
                  ((disk * tower * tower) list -> 'a) -> 'a
 * REQUIRES: p is a total function, L is a disk list where
 *           the disks are in descending order
 * ENSURES: hanoi_helper (L, t1, t2, t3) k ==> k M,
 * where M is a list of (disk, tower1, tower2) tuples representing
 * valid moves from tower1 to tower2. The sequence of moves in M
 * results in a movement of the whole tower from tower1 to tower2,
 * via tower3
 *)
fun hanoi_helper (L : disk list, t1 : tower, t2 : tower, t3 : tower)
    (k : (disk * tower * tower) list -> 'a) : 'a =
    case L of
      [] => k []
     |[d] => k [(d, t1, t2)]
     |d::ds => hanoi_helper (ds, t1, t3, t2) (fn ds' =>
               hanoi_helper ([d], t1, t2, t3) (fn ds'' =>
               hanoi_helper (ds, t3, t2, t1) (fn ds''' =>
               k (ds'@ds''@ds'''))))

val [] = hanoi_helper ([], TowerA, TowerB, TowerC) (fn x => x)
val [(Disk 1, TowerA, TowerB)] = hanoi_helper
                              ([Disk 1], TowerA, TowerB, TowerC) (fn x => x)
val [(Disk 1, TowerA, TowerC),(Disk 2, TowerA, TowerB),
     (Disk 1, TowerC, TowerB)] = hanoi_helper ([Disk 2, Disk 1], TowerA,
                                               TowerB, TowerC) (fn x => x)


(* hanoi : disk list * tower * tower * tower ->
                     ((disk * tower * tower) list -> 'a) -> 'a
 * REQUIRES: k is a total function, the disks in L are in ascending order
 * ENSURES: hanoi (L, tower1, tower2, tower3) k ==> k M,
 * where M is a list of (disk, tower1, tower2) tuples representing
 * valid moves from tower1 to tower2. The sequence of moves in M
 * results in a movement of the whole tower from tower1 to tower2,
 * via tower3
 *)
fun hanoi (L : disk list, tower1 : tower, tower2 : tower, tower3 : tower)
          (k : (disk * tower * tower) list -> 'a) : 'a =
    case L of
      [] => k []
     |[d] => k[(d, tower1, tower2)]
     |d::ds => let
                  val revList = List.rev(L)
               in
                 hanoi_helper(revList, tower1, tower2, tower3) k
               end

(* Task 5.1: findRoute : int depthTree -> (int * int) ->
                         ((int * int) -> 'a) -> (unit -> 'a) -> 'a
 * REQUIRES: dt is an int depthree, where the value at each node
 * is the cost (in hundreds of dollars) of making that step
 * ENSURES: findRoute dt (budget, stop) s k ==> s (cost, stop'),
 * where cost and stop' are the total cost of and the
 * depth of the leftmost branch that maximizes
 * the distance traveled, bounded by stop, while
 * keep cost under the budget
 *)
fun findRoute (dt : int depthTree) (budget : int, stop : int)
              (s : (int * int) -> 'a) (k: unit -> 'a) : 'a =
    case dt of
      DEmpty => k ()
     |DNode ( l, (cost, depth), r) =>
      (case Int.compare(depth, stop) of
         LESS =>
         (case Int.compare(cost, budget) of
            LESS => findRoute l ((budget - cost), stop) (fn (cost', stop') =>
                    s (cost' + cost, stop')) (fn () =>
                    findRoute r ((budget - cost), stop) (fn (cost', stop') =>
                    s (cost' + cost, stop')) k)
           |EQUAL => s (cost, depth)
           |GREATER => k())
         |EQUAL =>
          (case Int.compare (cost, budget) of
             GREATER => k()
            |_ => s (cost, depth))
         |GREATER => raise Fail "lel ain't happening")


(*    let
      fun helper (dt : int depthTree) (budget : int, stop : int)
                 (s: (int * int) -> (int * int) option) (compare : (int * int) option * (int * int) option -> (int * int) option)
                 (k : unit -> (int * int) option) : (int * int) option =
          case dt of
            DEmpty => k ()
           |DNode(l, (cost,depth),r) =>
            (case Int.compare(depth, stop) of
              LESS =>
              (case Int.compare(cost,budget) of
                 LESS => compare(helper l (budget - cost, stop) s compare
                                        (fn () => s ((budget - cost), depth)),
                                 helper r (budget - cost, stop) s compare
                                        (fn () =>s ((budget - cost), depth)))
                |EQUAL => s ((budget - cost), depth)
                |GREATER => k ())
            |EQUAL =>
              (case Int.compare (cost, budget) of
                 GREATER => k ()
                |_ => s ((budget - cost), depth))
            |GREATER => k ())
      fun optCompare (opt1 : (int * int) option, opt2 : (int * int) option):
          (int * int) option =
          case (opt1,opt2) of
            (NONE,NONE) => NONE
           |(SOME (x1, y1), SOME (x2,y2)) =>
            (case (Int.compare (y1, y2)) of
               LESS => opt2
              |_ => opt1)
           |(NONE,_) => opt2
           |(_,NONE) => opt1
      fun getOption (x : int, y : int) : (int * int) option =
          SOME (x, y)
    in
      (case dt of
        DEmpty => k ()
       |DNode (l, (cost, depth), r) => (case (helper dt (budget, stop)
                                                     getOption
                                                     optCompare (fn () => NONE)) of
                                          NONE => k ()
                                         |SOME (x,y) => s (budget - x, y)))
    end *)

val SOME(5,2) = findRoute (DNode(DNode(DNode(DEmpty,(1,3),DEmpty),(3,2),DEmpty),(2,1),DNode(DEmpty,(5,2),DEmpty))) (5,4) (fn (x,y) => SOME(x,y)) (fn () => NONE)
val NONE = findRoute (DNode(DNode(DNode(DEmpty,(1,3),DEmpty),(3,2),DEmpty),(2,1),DNode(DEmpty,(5,2),DEmpty))) (1,4) (fn (x,y) => SOME(x,y)) (fn () => NONE)
val 72 = findRoute (DNode(DNode(DNode(DEmpty,(37,3),DEmpty),(6,2),DNode(DEmpty,(24,3),DEmpty)),(7,1),DNode(DNode(DEmpty,(4,3),DEmpty),(13,2),DNode(DEmpty,(9,3),DEmpty)))) (24,3) (fn (x,y) => x*y) (fn () => 0)
val 0 = findRoute (DNode(DNode(DNode(DEmpty,(37,3),DEmpty),(6,2),DNode(DEmpty,(24,3),DEmpty)),(7,1),DNode(DNode(DEmpty,(4,3),DEmpty),(13,2),DNode(DEmpty,(9,3),DEmpty)))) (6,3) (fn (x,y) => x*y) (fn () => 0)
val 26 = findRoute (DNode(DNode(DNode(DEmpty,(37,3),DEmpty),(13,2),DNode(DEmpty,(24,3),DEmpty)),(7,1),DNode(DNode(DEmpty,(4,3),DEmpty),(6,2),DNode(DEmpty,(9,3),DEmpty)))) (19,2) (fn (x,y) => x*y) (fn () => 0)

(* Task 6.1: seatable:
 * REQUIRES:
 * ENSURES:
 *)
fun seatable (enemies : relationship list)
             (tables : int list)
             (s : assignment list -> 'a)
             (k : unit -> 'a): 'a = raise Fail "Unimplemented!"
