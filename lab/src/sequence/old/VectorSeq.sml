structure VectorSeq :> SEQUENCE =
struct
  type 'a seq = 'a vector
  
  exception Range
  
  val len = Vector.length
  
  fun nth s i = Vector.sub (s,i) handle Subscript => raise Range
  
  fun eseq x = (Vector.fromList [x]) handle _ => raise Range
  
  fun tabulate f n = (Vector.tabulate (n, f)) handle _ => raise Range
  fun mseq (x,n) = (tabulate (fn _ => x) n) handle _ => raise Range
  
  val map = Vector.map
  
  fun zip (s1,s2) = 
    if Vector.length s1 <> Vector.length s2 then raise Range
    else tabulate (fn i => (nth s1 i, nth s2 i)) (Vector.length s1)
  
  fun combine f s =
    case len s of
      0 => raise Range
    | 1 => nth s 0
    | n =>
        let
          (* contract *)
          val n2 = n div 2
          fun contract i = 
            if i >= n2 then nth s (i*2) 
            else f (nth s (i*2), nth s (i*2+1))
          val half = tabulate contract (n2 + (n mod 2))
        in
          (* solve *)
          combine f half
        end
  
  fun reduce f x s =
      if length s = 0 then x
      else f (x, combine f s)
  
  fun cons (x,s) = Vector.concat [eseq x, s]
  fun append (s1,s2) = Vector.concat [s1,s2]
  fun flatten ss = reduce append #[] ss
  
  fun scan f x s =
    case len s of
      0 => raise Range
    | 1 => eseq x
    | n => 
        let
          (* contract *)
          val n2 = n div 2
          fun contract i = 
            if i >= n2 then nth s (i*2) 
            else f (nth s (i*2), nth s (i*2+1))
          val half = tabulate contract (n2 + (n mod 2))
          
          (* solve *)
          val s' = scan f x half
          
          (* expand *)
          fun expand i =
            if i mod 2 = 0 then nth s' (i div 2)
            else f (nth s' (i div 2), nth s (i - 1))
        in
          tabulate expand n
        end

  (* TODO: don't use Unsafe.cast (?) *)
  
  val update_ : 'a vector -> int -> 'a -> unit =
    fn v => fn i => fn a => Array.update (Unsafe.cast v, i, a)
  
  fun update s is = 
    let
      (* make a copy *)
      val copy = tabulate (fn i => nth s i) (len s)
      
      (* update the copy directly *)
      val n = len is
      fun parloop i = 
        if i >= n then () else
        let val (j,x) = nth is i
        in 
          (update_ copy j x; parloop (i+1))
        end
    in
      (parloop 0; copy)
    end
  
  (*fun filter f s =
    Vector.fromList (Vector.foldr (fn (x,L) => if f x then x::L else L) [] s)*)
  
  fun filter f s =
    let
      val mask = map (fn x => if f x then 1 else 0) s
      val psum = scan (op +) 0 mask
      val last = (len psum) - 1
      val result = mseq (nth s 0, (nth psum last) + (nth mask last))
      fun parloop ~1 = ()
        | parloop i =
            if nth mask i = 1 then (update_ result (nth psum i) (nth s i); parloop (i-1))
            else parloop (i-1)
    in
      (parloop ((len mask) - 1); result)
    end
  
  (* index a b c -> [b, b+a, b+2a, ..., <c] *)
  fun idxs a b c = 
    let
      val l = ((c - a) div b) + 1
    in
      tabulate (fn i => a + i * b) l
    end
    
  (*val partition : 'a seq -> int seq -> 'a seq seq*)
  fun partition s is =
    let
      val n = len is
      val m = len s
      (* compute lengths of each partition *)
      val lens = tabulate (fn i => 
        if i = n-1 then m - (nth is i)
        else 
          let
            val a = nth is i
            val b = nth is (i+1)
          in
            if b > a andalso b < m then b - a
            else raise Range
          end) n
      (* build the partitions *)
      val parts = tabulate (fn j =>
        tabulate (fn i => nth s ((nth is j) + i)) (nth lens j)) n
    in
      parts
    end
  
  fun subseq s (l,r) = 
    if l < 0 orelse l > r then raise Range
    else tabulate (fn i => nth s (i+l)) (r-l)
  
  fun head s n = subseq s (0, n)
  
  fun tail s n = subseq s (len s - n, len s)
  
  fun toString (s, f) =
    let
      val n = len s
      val strs = tabulate (fn i => if i < (n-1) then (f (nth s i)) ^ "," else f (nth s i)) n
    in
      if n = 0 then "[]"
      else String.concat ["[", reduce String.^ "" strs, "]"]
    end
  
  fun valid s = true

  fun mkEmpty () = Vector.fromList []
end
