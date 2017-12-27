(* DRL, Spring 2012:
   adapting sequences to manticore.

   Any changes to ../sequencecore.sig and ../sequence.sig should be propagated here.
   
   differences from the SML signature:
   - manticore doesn't seem to allow exceptions in signatures?
     removed Range; functions raise whatever PArray raises instead.
   - manticore doesn't seem to allow include?
     inlined sequencecore
*)

signature SEQUENCE = 
sig
  type 'a seq

  val length : 'a seq -> int

  val nth    : int -> 'a seq -> 'a

  val tabulate : (int -> 'a) -> int -> 'a seq
  val filter : ('a -> bool) -> 'a seq -> 'a seq

  val map : ('a -> 'b) -> 'a seq -> 'b seq

  val reduce : (('a * 'a) -> 'a) -> 'a -> 'a seq -> 'a

  val mapreduce : ('a -> 'b) -> 'b -> ('b * 'b -> 'b) -> 'a seq -> 'b

  val toString : ('a -> string) -> 'a seq -> string

  val repeat  : int -> 'a -> 'a seq
  val zip     : ('a seq * 'b seq) -> ('a * 'b) seq
  val flatten : 'a seq seq -> 'a seq

  val empty : unit -> 'a seq
  val cons  : 'a -> 'a seq -> 'a seq

  val singleton : 'a -> 'a seq
  val append    : 'a seq -> 'a seq -> 'a seq

(*
  FIXME implement these:

  datatype 'a lview = Nil | Cons of 'a * 'a seq
  datatype 'a tview = Empty | Leaf of 'a | Node of 'a seq * 'a seq

  val showl : 'a seq -> 'a lview
  val hidel : 'a lview -> 'a seq

  val showt : 'a seq -> 'a tview
  val hidet : 'a tview -> 'a seq

  val split   : int -> 'a seq -> 'a seq * 'a seq  (* FIXME: document behavior when there are not enough elts *)
  val take    : int -> 'a seq -> 'a seq
  val drop    : int -> 'a seq -> 'a seq
*)


end
