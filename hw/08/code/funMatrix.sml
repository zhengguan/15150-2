structure FunVector : VECTOR =
struct
  exception Unimplemented
  exception outOfBounds

  type elem = int
  type vector = int * (int -> elem)

  (* inBounds : int -> int -> bool
   * REQUIRES: true
   * ENSURES: Given n is the dimension of a vector and i is an index,
   *          inBounds n i ==> true if i is a valid index for that dimension,
   *                   and ==> false otherwise
   *)
  fun inBounds (n : int) (i : int) : bool =
      (not (Int.compare(i, 0) = LESS)) andalso (Int.compare(i,n) = LESS)

  (* tabulate : (int -> elem) -> int -> vector
   * REQUIRES: n >= 0,
   *           f i ==> v where v is a value for all i >= 0, i < n
   * ENSURES : tabulate f n ==> a vector where entry i is given by
   *           f i, provided 0 <= i < n
   *)
  fun tabulate (f : (int -> elem)) (n : int) : vector =
      (n,(fn i => case (inBounds n i) of
                    true  => f i
                   |false => raise outOfBounds))

  (* dotprodHelp : (int -> elem) -> (int -> elem) -> int -> elem -> int -> elem
   * REQUIRES: for all i >= 0, i < n, (f1 i), (f2 i) must be valuable. Also,
   *           i <= n
   * ENSURES: dotprodHelp f1 f2 n s i ==> the sum of the products of (f1 i')
   *          and (f2 i') for i' >= i, i' < n
   *)
  fun dotprodHelp (f1 : int -> elem) (f2 : int -> elem) (n : int) (s : elem)
                  (i : int) : elem =
      case (n = i) of
        true  => s
       |false => dotprodHelp f1 f2 n (s + ((f1 i)*(f2 i))) (i + 1)

  (* dotprod : vector * vector -> elem
   * REQUIRES: v and w must have the same dimension, i.e. number of elements
   * ENSURES: dotprod (v, w) ==> the scalar dot product of the vectors,
   *          i.e. the sum of the product of the corresponding elements
   *          of each
   *)
  fun dotprod (v : vector, w : vector) : elem =
      let
         val (n1, f1) = v
         val (n2, f2) = w
      in
        case (n1 = n2) of
          false => raise outOfBounds
         |true  => dotprodHelp f1 f2 n1 0 0
      end

  (* length : vector -> int
   * REQUIRES: true
   * ENSURES: length v ==> the dimension of the vector, i.e. the
   *          number of elements in v
   *)
  fun length (v : vector) : int =
      let
         val (n, f) = v
      in
        n
      end

  (* eqHelp : (int -> elem) -> (int -> elem) -> int -> int -> bool
   * REQUIRES: for all i >= 0, i < n, (f1 i) and (f2 i) must be valuable. Also,
   *           i < n
   * ENSURES: eqHelp f1 f2 n i ==> true if for all i' >= i, i' < n,
   *          (f1 i') = (f2 i')
   *)
  fun eqHelp (f1 : int -> elem) (f2 : int -> elem) (n : int) (i : int) : bool =
      case (i = (n - 1)) of
        true  => (f1 i) = (f2 i)
       |false =>((f1 i) = (f2 i)) andalso (eqHelp f1 f2 n (i+1))

  (* eq : vector * vector -> bool
   * REQUIRES: true
   * ENSURES: eq (v, w) ==> true if v and w have the same dimensions and
   *          every element at the corresponding indices are the same
   *          for every index element of v and w
   *)
  fun eq (v : vector, w : vector) : bool =
      let
         val (n1, f1) = v
         val (n2, f2) = w
      in
        (n1 = n2) andalso (eqHelp f1 f2 n1 0)
      end
  (* nth : (vector * int) -> elem
   * REQUIRES: i is a valid index into v
   * ENSURES: nth (v, i) ==> the ith index element of v
   *)
  fun nth (v : vector, i : int) : elem =
      let
         val (n, f) = v
      in
        case (inBounds n i) of
          false => raise outOfBounds
         |true  => f i
      end
end

structure FunMatrix : MATRIX =
struct
  exception Unimplemented
  exception outOfBounds
  exception invalidDimensions

  structure Vector = FunVector

  type elem = Vector.elem
  type vector = Vector.vector
  type matrix = (int * int) * (int -> int -> elem)

  (* inBounds : int -> int -> bool
   * REQUIRES: true
   * ENSURES: Given n is the dimension of a vector and i is an index,
   *          inBounds n i ==> true if i is a valid index for that dimension,
   *                   and ==> false otherwise
   *)
  fun inBounds (n : int) (i : int) : bool =
      (not (Int.compare(i, 0) = LESS)) andalso (Int.compare(i,n) = LESS)

  (* tabulate : (int * int -> elem) -> int -> int -> matrix
   * REQUIRES: n >= 1, m >= 1,
              (f (i, j)) is valuable for all i >= 0, i < n, j >= 0, j < m
   * ENSURES: tabulate f n m ==> an n by m matrix for which each row i
   *          and column j, the element in the matrix is f (i,j)
   *)
  fun tabulate (f : (int * int) -> elem) (n : int) (m : int) : matrix =
      ((n,m),(fn i => case (inBounds n i) of
                        false => raise outOfBounds
                       |true  =>
             (fn j => case (inBounds m j) of
                        false => raise outOfBounds
                       |true  => f (i, j))))

  (* update : matrix -> (int * int) -> elem -> matrix
   * REQUIRES: i and j are valid indices for a row an column in M,
               M is a valid matrix
   * ENSURES: update M (i,j) v ==> a matrix identical to M, but with the
   *          element at the ith row and the jth column replaced with v
   *)
  fun update (M : matrix) (i : int, j : int) (v : elem) : matrix =
      let
         val ((n,m), f) = M
      in
         ((n,m),(fn i' =>
                (fn j' => (case (i' = i, j' = j) of
                             (true, true)  => v
                            |(_,_) => f i' j'))))
      end

  (* identity : int -> matrix
   * REQUIRES: n >= 1
   * ENSURES: identity n ==> the n by n identity matrix, in which
   *          all elements on the main diagonal are 1 and all other
   *          elements are 0
   *)
  fun identity (n : int) : matrix =
      ((n,n),(fn i => case (inBounds n i) of
                        false => raise outOfBounds
                       |true  =>
             (fn j => case (inBounds n j) of
                        false => raise outOfBounds
                       |true  => (case (i = j) of
                                    true  => 1
                                   |false => 0))))

  (* row : matrix -> int -> vector
   * REQUIRES: i is a valid row index for the matrix M, M is a valid matrix
   * ENSURES: row M i ==> the vector of dimension m representing the
   *          ith row of the matrix M, given M has dimensions (n,m)
   *)
  fun row (M : matrix) (i : int) : vector =
      let
         val ((_,m),f) = M
      in
        (m, (fn j => f i j))
      end

  (* col : matrix -> int -> vector
   * REQUIRES: j is a valid column index for the matrix M, M is a valid matrix
   * ENSURES: col M j ==> the vector of dimension n representing the
   *          jth column of the matrix M, given M has dimensions (n,m)
   *)
  fun col (M : matrix) (j : int) : vector =
      let
         val ((n,_),f) = M
      in
        (n, (fn i => f i j))
      end

  (* size : matrix -> (int * int)
   * REQUIRES: M is a valid matrix
   * ENSURES: size M ==> (n,m), where n is the number of rows in M and
   *          m is the number of columns in M
   *)
  fun size (M : matrix) : (int * int) =
      let
         val ((n,m),_) = M
      in
        (n,m)
      end

  (* transpose : matrix -> matrix
   * REQUIRES: M is a valid matrix
   * ENSURES: transpose M ==> M^T, the tranpose of M where
   *          the rows of M are the columns of M^T, and
   *          the columns of M are the rows of M^T
   *)
  fun transpose (M : matrix) : matrix =
      let
         val ((n,m),f) = M
      in
        ((m,n),(fn i =>
               (fn j => (f j i))))
      end

  (* add : matrix * matrix -> matrix
   * REQUIRES: A and B are valid matrices with the same dimensions
   * ENSURES: add (A,B) ==> M, a matrix with the same dimensions as A and B,
   *          where for each row i and column j in M,
   *          the element M(i,j) = A(i,j) + B(i,j)
   *)
  fun add (A : matrix, B : matrix) : matrix =
      let
         val ((n1,m1),f1) = A
         val ((n2,m2),f2) = B
      in
         case (n1 = n2, m1 = m2) of
           (true, true) =>
                  ((n1, m1), (fn i =>
                             (fn j => ((f1 i j) + (f2 i j)))))
          |(_,_) => raise invalidDimensions
      end

  (* subtract : matrix * matrix -> matrix
   * REQUIRES: A and B are valid matrices with the same dimensions
   * ENSURES: subtract (A,B) ==> M, a matrix with the
              same dimensions as A and B, where for each row i and column j
              in M, the element M(i,j) = A(i,j) + B(i,j)
   *)
  fun subtract (A : matrix, B : matrix) : matrix =
      let
         val ((n1,m1),f1) = A
         val ((n2,m2),f2) = B
      in
         case (n1 = n2, m1 = m2) of
           (true, true) =>
                  ((n1, m1), (fn i =>
                             (fn j => ((f1 i j) - (f2 i j)))))
          |(_,_) => raise invalidDimensions
      end

  (* mult : matrix * matrix -> matrix
   * REQUIRES: A and B are valid matrices, where the
               number of columns in A is the same as the number of rows in B
   * ENSURES: mult (A,B) ==> A x B, the matrix product of A and B
   *)
  fun mult (A : matrix, B : matrix) : matrix =
      let
         val ((n1,m1),f1) = A
         val ((n2,m2),f2) = B
      in
         case (m1 = n2) of
           false => raise invalidDimensions
          |true  => ((n1,m2),(fn i =>
                             (fn j =>
                             Vector.dotprod((row A i), (col B j)))))
      end

  (* eqHelp : matrix -> matrix -> int -> bool
   * REQUIRES: A and B are valid matrices with the same dimensions,
   *           i is less than the number of rows in A and B
   * ENSURES: eqHelp A B i ==> true if, given A and B are n by m matrices,
   *          for all i' >= i, i' < n, every element in the i'th row of
   *          A and B is the same,
   *                   and ==> false otherwise
   *)
  fun eqHelp (A : matrix) (B : matrix) (i : int) : bool =
      let
         val (n,m) = size A
      in
        case (i = (n-1)) of
          true  => (Vector.eq((row A i), (row B i)))
         |false => (Vector.eq((row A i), (row B i))) andalso (eqHelp A B (i+1))
      end

  (* eq : matrix * matrix -> bool
   * REQUIRES: true
   * ENSURES: eq (A, B) ==> true if A and B are the same matrix, i.e.
              for each row i and column j,  A(i,j) = B(i,j)
                    and ==> false otherwise
   *)
  fun eq (A : matrix, B : matrix) : bool =
      let
        val (n1, m1) = size A
        val (n2, m2) = size B
      in
        (n1 = n2) andalso (m1 = m2) andalso (eqHelp A B 0)
      end
end
