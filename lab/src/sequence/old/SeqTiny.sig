signature SEQUENCE =
sig

  type 'a seq

  exception Range

  val length : 'a seq -> int (* Work/Span: O(1) *)
  val nth    : 'a seq -> int -> 'a (* Work/Span: O(1) *)

  val tabulate : (int -> 'a) -> int -> 'a seq
    (* Evaluate each position in parallel *)

  val filter : ('a -> bool) -> 'a seq -> 'a seq
    (* Evaluate each position in parallel, then combine *)

  val map : ('a -> 'b) -> 'a seq -> 'b seq
    (* Evaluate each position in parallel *)
  val reduce : (('a * 'a) -> 'a) -> 'a -> 'a seq -> 'a
  val mapreduce : ('a -> 'b) -> 'b -> ('b * 'b -> 'b) -> 'a seq -> 'b

  (*
    Could be derived forms if we use views from Seq.sig
  *)

  val toString : 'a seq * ('a -> string) -> string

  val repeat  : ('a * int) -> 'a seq
  val zip     : ('a seq * 'b seq) -> ('a * 'b) seq
  val flatten : 'a seq seq -> 'a seq
  val split   : 'a seq -> int -> 'a seq * 'a seq
  val head    : 'a seq -> int -> 'a seq
  val tail    : 'a seq -> int -> 'a seq (* comutes the last n elements, not what is leftover when you take n *)

  val empty : unit -> 'a seq
  val cons  : ('a * 'a seq) -> 'a seq

  val singleton : 'a -> 'a seq
  val append    : ('a seq * 'a seq) -> 'a seq

  val update : 'a seq -> (int * 'a) seq -> 'a seq

end
