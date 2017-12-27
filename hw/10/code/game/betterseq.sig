signature BETTERSEQ =
sig

  include SEQUENCE
  val insert : 'a -> 'a seq -> int -> 'a seq
  val mapIdx : ('a * int -> 'b) -> 'a seq -> 'b seq
  val extractSomes : 'a option seq -> 'a seq

end
