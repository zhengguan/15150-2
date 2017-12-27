structure SeqUtils :
sig
  (* assumes sequence is non-empty *)
  val reduce1 : ('a * 'a -> 'a) -> 'a Seq.seq -> 'a
  val null : 'a Seq.seq -> bool

  val seqToList : 'a Seq.seq -> 'a list
  val seqFromList : 'a list -> 'a Seq.seq
end =
struct
  val reduce1 = Seq.reduce1
  val null = Seq.null
  val seqToList = Seq.toList
  val seqFromList = Seq.fromList
end

structure TestUtil =
struct

    val test = Seq.cons (((), 84)) (Seq.cons ((), 32) (Seq.cons ((), ~4) (Seq.cons ((), 0) (Seq.empty()))))
(* 
    structure O = OrderUtils (PairSecondOrder (struct type left = unit structure Right = IntLt end))
    val test = SeqUtils.reduce1 O.min test
*)
    val tmin = SeqUtils.reduce1 (fn (a as (_,x), b as (_,y)) => if x < y then a else b) test

    val true = SeqUtils.null (Seq.empty ())
    val false = SeqUtils.null (Seq.tabulate (fn i => i) 1)
end
