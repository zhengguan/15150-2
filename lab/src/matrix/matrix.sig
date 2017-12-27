signature MATRIX =
sig
  structure Seq : SEQUENCE

  type 'a matrix

  (* coordinates are always (column, row), or (width, height)
     e.g. Matrix.repeat - (x, y)
     makes a matrix with x columns and y rows

     width = number of columns
     height = number of rows
  *)

  (* Returns (width, height) of the matrix *)
  val size : 'a matrix -> (int * int)
  (* Returns the value of the matrix at the given subscripts (indexed from 0) *)
  val sub  : 'a matrix -> (int * int) -> 'a

  (* Returns the matrix with the given dimensions and the given value at each
   * position. *)
  val repeat : 'a -> (int * int) -> 'a matrix

  (* tabulate f (w, h) returns the matrix with the given dimensions such that
   * the value in position (i,j) is f(i,j) where (i, j) is (col, row) *)
  val tabulate : (int * int -> 'a) -> (int * int) -> 'a matrix
  (* maps the given function to each value in the matrix *)
  val map : ('a -> 'b) -> 'a matrix -> 'b matrix

  (* Returns the matrix with the value at the given subscript updated to the
   * given value *)
  val update : 'a matrix -> ((int * int) * 'a) -> 'a matrix

  (* reduce rowf rowb colf colb combines the matrix down to a single item,
   * first reducing the columns with colf and colb
   * then the rows with rowf and rowb.
   *)
  val reduce : ('a * 'a -> 'a) -> 'a -> ('a * 'a -> 'a) -> 'a -> 'a matrix -> 'a

  (* Returns the subscripts of the elements in the matrix for which the given
   * predicate evaluates to true *)
  val matching_subs : ('a -> bool) -> 'a matrix -> (int * int) Seq.seq

  (* Returns the sequence of rows in the matrix *)
  val rows : 'a matrix -> ('a Seq.seq) Seq.seq
  (* Returns the sequence of columns in the matrix *)
  val cols : 'a matrix -> ('a Seq.seq) Seq.seq

  (* Returns the matrix from a sequence of rows *)
  val from_rows : ('a Seq.seq) Seq.seq -> 'a matrix
  (* Returns the sequence from a sequence of columns *)
  val from_cols : ('a Seq.seq) Seq.seq -> 'a matrix

  (* diags1 and diags2 returns the two sequences of diagonals in the matrix,
   * from bottom left to top right and from top left to bottom right. *)
  val diags1 : 'a matrix -> ('a Seq.seq) Seq.seq
  val diags2 : 'a matrix -> ('a Seq.seq) Seq.seq

  (* returns the enumerated row/col/diagonal of m containg the position s
   *   pecified
   * For example rowFrom (tabulate (fn x => "a") (4,4)) (2,3) returns
   *  <("a",(0,3)), ("a",(1,3)), ("a",(2,3)), ("a",(3,3))> *)
  val rowFrom : 'a matrix -> (int * int) -> ('a * (int*int)) Seq.seq
  val colFrom : 'a matrix -> (int * int) -> ('a * (int*int)) Seq.seq
  val diags1From : 'a matrix -> (int * int) -> ('a * (int*int)) Seq.seq
  val diags2From : 'a matrix -> (int * int) -> ('a * (int*int)) Seq.seq

  (* ENSURES: adds (col,row) tags to the elements of m *)
  val enum : 'a matrix -> ('a * (int*int)) matrix

  (* REQUIRES: startpos, endpos in bounds of matrix and on the same
   *   row/col/diag *)
  (* ENSURES: between m startpos endpos returns the elements of
   *    m between startpos and endpos noninclusive. *)
  val between : 'a matrix -> (int * int) -> (int * int) -> 'a Seq.seq
end
