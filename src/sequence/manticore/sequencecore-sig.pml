(* DRL, Spring 2012:
   adapting sequences to manticore.

   Any changes to ../sequencecore.sig should be propagated here.
   
   differences from the SML signature:
   - manticore doesn't seem to allow exceptions in signatures?

*)

signature SEQUENCECORE =
sig
  type 'a seq

(*  exception Range *)

  val length : 'a seq -> int
  val nth    : int -> 'a seq -> 'a

  val tabulate : (int -> 'a) -> int -> 'a seq
  val filter : ('a -> bool) -> 'a seq -> 'a seq

  val map : ('a -> 'b) -> 'a seq -> 'b seq
  val reduce : (('a * 'a) -> 'a) -> 'a -> 'a seq -> 'a

  datatype 'a lview = Nil | Cons of 'a * 'a seq
  datatype 'a tview = Empty | Leaf of 'a | Node of 'a seq * 'a seq

  val showl : 'a seq -> 'a lview
  val hidel : 'a lview -> 'a seq

  val showt : 'a seq -> 'a tview
  val hidet : 'a tview -> 'a seq
end
