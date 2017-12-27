structure VectorSeq :> SEQUENCE =
struct
  type 'a seq = 'a vector

  exception Range

  val length = Vector.length

  fun nth s i = Vector.sub (s,i) handle Subscript => raise Range

  fun singleton x = (Vector.fromList [x]) handle _ => raise Range

  exception PassAlong
  fun tabulate f n =
      Vector.tabulate (n, fn x => (f x handle Size => raise PassAlong))
      handle Size => raise Range | PassAlong => raise Size

  fun repeat (x,n) = (tabulate (fn _ => x) n) handle _ => raise Range

  val map = Vector.map

  fun zip (s1,s2) =
    if Vector.length s1 <> Vector.length s2 then raise Range
    else tabulate (fn i => (nth s1 i, nth s2 i)) (Vector.length s1)

  fun combine f s =
    case length s of
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

  fun mapreduce l e n s = reduce n e (map l s)

  fun cons (x,s) = Vector.concat [singleton x, s]
  fun append (s1,s2) = Vector.concat [s1,s2]
  fun flatten ss = reduce append (Vector.fromList []) ss

  (* Faster and _safe_ version suggested by Michael Sullivan. *)
  fun vecToArray v =
      let val l = Vector.length v
      in if l = 0 then Array.fromList []
         else let val a = Array.array (l, Vector.sub (v, 0))
              in Array.copyVec { src = v, dst = a, di = 0 }; a
              end
      end

  fun update s is =
      let val a = vecToArray s
          val _ = Vector.app (fn (i, x) => Array.update (a, i, x)) is
      in Array.vector a
      end

  fun filter f s =
      Vector.fromList
          (Vector.foldr (fn (e, xs) => if f e then e::xs else xs) [] s)

  (* index a b c -> [b, b+a, b+2a, ..., <c] *)
  fun idxs a b c =
    let
      val l = ((c - a) div b) + 1
    in
      tabulate (fn i => a + i * b) l
    end

  fun subseq s (l,r) =
    if l < 0 orelse l > r then raise Range
    else tabulate (fn i => nth s (i+l)) (r-l)

  fun head s n = subseq s (0, n)

  fun tail s n = subseq s (length s - n, length s)

  fun split s n = (head s n, tail s n) (* DRL FIXME: this isn't right, because tail is the last n elements, not what's leftover *)

  fun toString (s, f) =
    let
      val n = length s
      val strs = tabulate (fn i => if i < (n-1) then (f (nth s i)) ^ "," else f (nth s i)) n
    in
      if n = 0 then "<>"
      else String.concat ["<", reduce String.^ "" strs, ">"]
    end

  fun valid s = true

  fun empty () = Vector.fromList []
end
