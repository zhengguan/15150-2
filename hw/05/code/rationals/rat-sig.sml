signature RATIONAL =
sig
   type t

   val negate : t -> t
   val inverse : t -> t
   val plus : t * t -> t
   val times : t * t -> t
   val divide : t * t -> t
   val subtract : t * t -> t
   val fromPair : int * int -> t
   val fromInt : int -> t
   val toString : t -> string
   val compare : t * t -> order
end

