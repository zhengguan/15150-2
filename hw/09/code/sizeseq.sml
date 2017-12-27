structure SizeSeq : TSEQ =
struct

    datatype 'a sseq = Empty
                     | Leaf of 'a
                     | Node of 'a sseq * int * 'a sseq

    type 'a seq = 'a sseq
    exception Range

    (* length : 'a seq -> int
     * REQUIRES: S is a balanced sseq
     * ENSURES: length S ==> the number of elements in S
     *)
    fun length (S : 'a seq) : int =
        case S of
          Empty => 0
         |Leaf _ => 1
         |(Node(L, size, R)) => size

    (* nth : int -> 'a seq -> 'a
     * REQUIRES: i >= 0, i < (length S), S is a balanced, non-empty sseq
     * ENSURES: nth i S ==> the ith element in S, raises Range if i is invalid
     *)
    fun nth (i : int) (S : 'a seq) : 'a =
        case (i, S) of
          (_, Empty) => raise Range
         |(0, Leaf x) => x
         |(_, Leaf _) => raise Range
         |(_, Node(L, size, R)) =>
             (case ((size mod 2) = 0) of
                true  => (case (i < (size div 2)) of
                           true  => (nth i L)
                          |false => (nth (i - (size div 2)) R))
               |false => (case (i > (size div 2)) of
                           false => (nth i L)
                          |true  => (nth (i - ((size div 2) + 1)) R)))

    (* tabulate : (int -> 'a) -> int -> 'a seq
     * REQUIRES: n >= 0, f is total
     * ENSURES: tabulate f n ==> a balanced 'a seq with length n, where the
     *          ith element of the sequence is equivalent to (f i)
     *)
    fun tabulate (f : int -> 'a) (n : int) : 'a seq =
        case n of
          0 => Empty
         |1 => Leaf (f 0)
         |_ => (case (n mod 2 = 0) of
                  true  =>
                     Node((tabulate f (n div 2)),
                          n,
                          (tabulate (fn i => f (i + (n div 2))) (n div 2)))
                 |false =>
                   Node((tabulate f ((n div 2)+1)),
                        n,
                        (tabulate (fn i => f (i + ((n div 2)+1))) (n div 2))))

    val f1 = (fn i => (i+1))
    val f2 = (fn n => n + 42)
    val f3 = (fn n => (n+1) * (n+1))
    val 0 = length (tabulate f1 0)
    val 4 = length (tabulate f1 4)
    val 9 = length (tabulate f3 9)
    val 42 = nth 0 (tabulate f2 5)
    val 46 = nth 4 (tabulate f2 5)
    val 3 = nth 2 (tabulate f1 5)
    val 4 = nth 3 (tabulate f1 6)
    val 100 = nth 5 (tabulate f3 5) handle Range => 100
    val 100 = nth 9 (tabulate f3 10)

end
