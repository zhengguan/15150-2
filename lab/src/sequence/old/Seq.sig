signature SEQUENCE =
sig

  type 'a seq

  exception Range

  val length : 'a seq -> int
  val nth    : 'a seq -> int -> 'a

  val tabulate : (int -> 'a) -> int -> 'a seq

  val filter : ('a -> bool) -> 'a seq -> 'a seq

  (*
    XXX: should we have map and reduce be derived forms or not?
      - efficiency problems if we do?
  *)

  val map : ('a -> 'b) -> 'a seq -> 'b seq
  val reduce : (('a * 'a) -> 'a) -> 'a -> 'a seq -> 'a
  val mapreduce : ('a -> 'b) -> 'b -> ('b * 'b -> 'b) -> 'a seq -> 'b

  (* Views *)

  datatype 'a lview = Nil | Cons of 'a * 'a seq
  datatype 'a tview = Empty | Leaf of 'a | Node of 'a seq * 'a seq

  val showl : 'a seq -> 'a lview
  val hidel : 'a lview -> 'a seq

  val showt : 'a seq -> 'a tview
  val hidet : 'a tview -> 'a seq

  (*
    Could be derived forms unless there is some reason not to (efficiency?)
    (maybe move to some utility structure?)
  *)

  val toString : 'a seq * ('a -> string) -> string

  val repeat  : ('a * int) -> 'a seq
  val zip     : ('a seq * 'b seq) -> ('a * 'b) seq
  val flatten : 'a seq seq -> 'a seq
  val split   : 'a seq -> int -> 'a seq * 'a seq
  val head    : 'a seq -> int -> 'a seq
  val tail    : 'a seq -> int -> 'a seq

  val empty : unit -> 'a seq
  val cons  : ('a * 'a seq) -> 'a seq

  val singleton : 'a -> 'a seq
  val append    : ('a seq * 'a seq) -> 'a seq

  (*

  FIXME: From old interface - do we need these yet?

  val scan : (('a * 'a) -> 'a) -> 'a -> 'a seq -> 'a seq
  val idxs : int -> int -> int -> int seq
  val partition : 'a seq -> int seq -> 'a seq seq
  val update : 'a seq -> (int * 'a) seq -> 'a seq
  val subseq : 'a seq -> (int * int) -> 'a seq
  *)

end
