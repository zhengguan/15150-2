signature HW05 =
sig
  type poly = int -> Rational.t

  val differentiate : poly -> poly
  val integrate : poly -> Rational.t -> poly
  val transpose : 'a list list -> 'a list list
  val all_available : (int * int) list list -> (int * int) list
  val all_good_available : (int * int) list list * int -> (int * int) list
  val extract : ('a -> bool) * 'a list -> ('a * 'a list) option
  val extractMany : ('a * 'a -> bool) * 'a list * 'a list -> ('a list) option
  val concat : 'a list list -> 'a list

end
