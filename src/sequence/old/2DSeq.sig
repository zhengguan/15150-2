signature SEQ2D =
sig
  structure Seq : SEQUENCE

  type 'a seq2d
  type 'a seq

  val size : 'a seq2d -> (int * int)
  val sub  : 'a seq2d -> (int * int) -> 'a
  
  val repeat : 'a -> (int * int) -> 'a seq2d

  val tabulate : (int * int -> 'a) -> (int * int) -> 'a seq2d
  val map : ('a -> 'b) -> 'a seq2d -> 'b seq2d

  val reduce : (('a * 'a) -> 'a) -> 'a -> 'a seq2d -> 'a
  val update : 'a seq2d -> ((int * int) * 'a) seq -> 'a seq2d

  (* Filtering makes it 1D instead of 2D *)
  val filter : ('a -> bool) -> 'a seq2d -> 'a seq
end
