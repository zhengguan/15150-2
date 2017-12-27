signature SEQUENCE =
sig
   type 'a seq

  exception Range of string

  (* length <x_0, ..., x_(n-1)> == n *)
  val length : 'a seq -> int

  (* Added in 2014: *)
  val null : 'a seq -> bool

  (* nth i <x_0, ..., x_(n-1)> == x_i if 0 <= i < n, raises Range otherwise *)
  val nth    : int -> 'a seq -> 'a

  (* tabulate f n == <f 0, ..., f n-1> *)
  val tabulate : (int -> 'a) -> int -> 'a seq

  (* filter p <x_0, ..., x_(n-1)> == <x_i | p x_i == true>
   * that is, filter p s computes the sequence of all elements si of s such that
   * p si == true, in the original order. *)
  val filter : ('a -> bool) -> 'a seq -> 'a seq

  (* map f <x_0, ..., x_(n-1)> == <f x_0, ..., f x_(n-1)> *)
  val map : ('a -> 'b) -> 'a seq -> 'b seq

  (* reduce op b <x_0, ..., x_(n-1)> == x_0 op x2 ... op x_(n-1),
   * that is, reduce applies the given function between all elements of the
   * input sequence, using b as the base case. *)
  val reduce : (('a * 'a) -> 'a) -> 'a -> 'a seq -> 'a

  datatype 'a lview = Nil | Cons of 'a * 'a seq
  datatype 'a tview = Empty | Leaf of 'a | Node of 'a seq * 'a seq

  val showl : 'a seq -> 'a lview
  val hidel : 'a lview -> 'a seq

  val showt : 'a seq -> 'a tview
  val hidet : 'a tview -> 'a seq

  (* Added in 2014: *)
  val toList : 'a seq -> 'a list
  val fromList : 'a list -> 'a seq


  (* mapreduce l e n s == reduce n e (map l s) *)
  val mapreduce : ('a -> 'b) -> 'b -> ('b * 'b -> 'b) -> 'a seq -> 'b

  (* Intuitively toString computes the string representation of a sequence
   * in the same style as the string for a list, using the given function to
   * convert each element to a string. More precisely:
   * toString elmToStr <x_0, ..., x_(n-1)> == "[" ^ elmToStr x_0 ^ ", " ... ^ elmToStr x_(n-1) ^ "]". *)
  val toString : ('a -> string) -> 'a seq -> string

  (* repeat n x == <x_0, ..., x_(n-1)> such that all x_i == x *)
  val repeat  : int -> 'a -> 'a seq

  (* zip (<x_0, ..., x_(n-1)>, <y_0, ..., y_(m-1)>) == <(x_0, y_0), ..., (x_k, y_k)> where
   * k = min(n,m). That is, zip truncates the longer sequence if needed *)
  val zip     : ('a seq * 'b seq) -> ('a * 'b) seq

  (* flatten ss == reduce append <> ss (the sequence analog of concat).
   * See below for the specification of append. *)
  val flatten : 'a seq seq -> 'a seq

  (* split k <x_0,...x_(k-1),x_k...x_(n-1)> == (<x_0,...x_(k-1)>, <x_k,...x_(n-1)>)
     (so the left result has length k)
     if the sequence has at least k elements

     or raises Range otherwise
  *)
  val split   : int -> 'a seq -> 'a seq * 'a seq

  (* take k <x_0,...x_(k-1),x_k...x_(n-1)> == <x_0,...x_(k-1)>
     drop k <x_0,...x_(k-1),x_k...x_(n-1)> == <x_k,...x_(n-1)>
     if the sequence has at least k elements

     or raise Range otherwise
     *)
  val take    : int -> 'a seq -> 'a seq
  val drop    : int -> 'a seq -> 'a seq

  (* empty () == <> *)
  val empty : unit -> 'a seq

  (* cons x_0 <x_1, ..., x_(n-1)> == <x_0, ..., x_(n-1)> *)
  val cons  : 'a -> 'a seq -> 'a seq

  (* singleton x == <x> *)
  val singleton : 'a -> 'a seq

  (* append (<x_0, ..., x_(n-1)>,
             <y_0, ..., y_(m-1)> == <x_0, ..., x_(n-1), y_0, ..., y_(m-1)>) *)
  val append    : ('a seq * 'a seq) -> 'a seq

  (* Added in 2014, assumes sequence is non-empty: *)
  val reduce1 : ('a * 'a -> 'a) -> 'a seq -> 'a

  (* Added in Spring 2016 - szo *)
  (* Updates sequence S at index i with element e *)
  (* Raises Subscript if out of bounds *)
  val update : ('a seq * int * 'a) -> 'a seq

  (* DRL, Spring 2012:
     made some interface-level changes to currying/argument order.
     these still need to get pushed through the HWs and labs.

     what changed:
     toString: swapped argument order and curried
     repeat: curried
     nth/split/take/drop: integer first
     append : curried    --- this got changed back to using a pair argument in Spring 2014
     cons: curried
   *)
end
