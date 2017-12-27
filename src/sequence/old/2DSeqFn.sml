functor Seq2DFn (Sequence : SEQUENCE) : SEQ2D =
struct
  structure Seq = Sequence
  type 'a seq = 'a Seq.seq

  (* row major ordering, keep track of x dimension *)
  type 'a seq2d = 'a Seq.seq * int

  fun to_pair x n = (n mod x, n div x)
  fun from_pair x (i,j) = (x*i)+j

  fun size (s,x) = (x, (Seq.length s) div x)

  fun sub (s,x) ij = Seq.nth s (from_pair x ij)

  fun repeat a (x,y) = (Seq.repeat (a,x*y), x)
  
  fun tabulate f (x,y) = (Seq.tabulate (f o (to_pair x)) (x*y), x)

  fun map f (s,x) = (Seq.map f s,x)

  fun reduce f a (s,_) = Seq.reduce f a s

  fun update (s,x) ind =
    (Seq.update s (Seq.map (fn (ij,a) => (from_pair x ij, a)) ind), x)

  fun filter f (s, _) = Seq.filter f s

end
