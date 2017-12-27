structure MatrixTests =
struct
  structure M = FunMatrix

  structure Vector = M.Vector

  (* fromLList : int list list -> M.matrix
   * REQUIRES: L is a list of int lists with all inner lists the same length
   * ENSURES: fromLList L returns a matrix specified by L, in row major-order
   *)
  fun fromLList L =
    case List.length L of
      0 => M.tabulate (fn _ => raise Fail "Input Matrix has too few rows") 0 0
    | n => let
             val m = List.length (List.hd L)
           in
             M.tabulate (fn (i, j) =>
               if i < 0 orelse j < 0 orelse i >= n orelse j >= m
               then raise Fail "Input Matrix has bad columns"
               else List.nth (List.nth (L, i), j)) n m
           end

  (* vecFromList : int list -> Vector.vector
   * REQUIRES: true
   * ENSURES: vecFromList L returns a vector containing exactly the elements
   *    of L
   *)
  fun vecFromList (L : int list) : Vector.vector =
    Vector.tabulate (fn i => List.nth (L, i)) (List.length L)

   (* tests for Matrix go here  *)
  (* First, let's test vectors *)
  val testList1 = [2, 1, 0]
  val testList2 = [1, 5, 0]
  val true = Vector.eq((Vector.tabulate (fn i => (case i of
                                                 0 => 2
                                                |1 => 1
                                                |_ => 0)) 3),
                       (vecFromList testList1))
  val false = Vector.eq((Vector.tabulate (fn i => (case i of
                                                 0 => 2
                                                |1 => 1
                                                |_ => 0)) 3),
                       (vecFromList testList2))
  val true = Vector.eq((Vector.tabulate (fn i => (case i of
                                                 0 => 1
                                                |1 => 5
                                                |_ => 0)) 3),
                       (vecFromList testList2))
  val true = Vector.eq((Vector.tabulate (fn i => 1) 1), (vecFromList [1]))
  val 7 = Vector.dotprod((vecFromList testList1),
                         (vecFromList testList2))
  val 1001 = Vector.dotprod((vecFromList testList1),
                            (vecFromList (4::testList2)))
          handle outOfBounds => 1001
  val 4 = Vector.length (Vector.tabulate (fn i => (case i of
                                                 0 => 2
                                                |1 => 1
                                                |2 => 0
                                                |_ => 5)) 4)
  val 42 = Vector.nth ((Vector.tabulate (fn i => (case i of
                                                 0 => 2
                                                |1 => 1
                                                |2 => 0
                                                |_ => 42)) 4),3)
  val 100  = Vector.nth ((Vector.tabulate (fn i => (case i of
                                                 0 => 2
                                                |1 => 1
                                                |2 => 0
                                                |_ => 42)) 4),5)
             handle outOfBounds => 100
  (* Now let's test matrices! *)
  val testLList1 = [[1, 2]]
  val testLList2 = [[1],
                    [2]]
  val testLList3 = [[1, 2],
                    [3, 4]]
  val testLList4 = [[1, 3],
                    [2, 4]]
  val testLList5 = [[1,2],
                    [3,4],
                    [5,6]]
  val testLList6 = [[1,3,5],
                    [2,4,6]]
  val true = M.eq((M.tabulate (fn (i,j) =>
                           (case (i,j) of
                                 (0,0) => 1
                                |(_,_) => 2)) 1 2),
                  fromLList testLList1)
  val false = M.eq((M.tabulate (fn (i,j) =>
                            (case (i,j) of
                                  (0,0) => 1
                                 |(_,_) => 2)) 1 2),
                  fromLList testLList2)
  val true = M.eq((M.tabulate (fn (i,j) =>
                           (case (i,j) of
                                 (0,0) => 1
                                |(_,_) => 2)) 2 1),
                  fromLList testLList2)
  val true = M.eq((M.tabulate (fn (i,j) =>
                           (case (i,j) of
                                 (0,0) => 1
                                |(0,1) => 2
                                |(1,0) => 3
                                |(_,_) => 5)) 2 2),
                  (M.update (fromLList testLList3) (1,1) 5))
  val false = M.eq((M.tabulate (fn (i,j) =>
                           (case (i,j) of
                                 (0,0) => 1
                                |(0,1) => 2
                                |(1,0) => 3
                                |(_,_) => 4)) 2 2),
                  (M.update (fromLList testLList3) (1,1) 5))
  val true = M.eq(M.transpose(fromLList testLList1),
                  (fromLList testLList2))
  val true = M.eq(M.transpose(fromLList testLList3),
                  (fromLList testLList4))
  val true = M.eq(M.transpose(fromLList testLList5),
                  (fromLList testLList6))
  val true = M.eq(M.transpose(M.transpose(fromLList testLList5)),
                  (fromLList testLList5))
  val true = M.eq(M.mult((fromLList testLList5),(M.identity 2)),
                  (fromLList testLList5))
  val true = M.eq(M.mult((fromLList testLList5),(M.identity 3)),
                  (fromLList testLList6))
      handle invalidDeminsions => true
  val (1,2) = M.size(fromLList testLList1)
  val (2,1) = M.size(fromLList testLList2)
  val (3,2) = M.size(fromLList testLList5)
  val testLList7 = [[1,3],
                    [1,0],
                    [1,2]]
  val testLList8 = [[0,0],
                    [7,5],
                    [2,1]]
  val testLList9 = [[1,3],
                    [8,5],
                    [3,3]]
  val testLList10 = [[~1,~3],
                     [6,5],
                     [1,~1]]
  val true = M.eq(M.add((fromLList testLList7),(fromLList testLList8)),
                  (fromLList testLList9))
  val true = M.eq(M.subtract((fromLList testLList8),(fromLList testLList7)),
                  (fromLList testLList10))
  val true = M.eq(M.add((fromLList testLList10),(fromLList testLList7)),
                  (fromLList testLList8))
  val true = Vector.eq((M.row (fromLList testLList6) 1),
                       (vecFromList [2,4,6]))
  val true = Vector.eq((M.row (fromLList testLList10) 2),
                       (vecFromList [1, ~1]))
  val true = Vector.eq((M.col (fromLList testLList9) 0),
                       (vecFromList [1,8,3]))
  val true = Vector.eq((M.col (fromLList testLList6) 2),
                       (vecFromList [5,6]))
  val testLList11 = [[5,11,17],
                     [11,25,39],
                     [17,39,61]]
  val true = M.eq((M.mult((fromLList testLList5),(fromLList testLList6))),
                   (fromLList testLList11))
  val testLList12 = [[5,11],
                     [11,25]]
  val true = M.eq((M.mult((fromLList testLList3),(fromLList testLList4))),
                   (fromLList testLList12))
  val false = M.eq((M.mult((fromLList testLList3),(fromLList testLList4))),
                   (fromLList testLList11))
end
