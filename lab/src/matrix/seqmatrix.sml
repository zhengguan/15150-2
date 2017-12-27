functor SeqMatrix (S : SEQUENCE) : MATRIX =
struct
    structure Seq = S

    datatype 'a absmat = M of ('a Seq.seq) Seq.seq

    (* this doesn't actually work, because the data is column-major, so it's
     * not a part of the public interface *)
    fun toString (e2s: 'a -> string) (M cols) =
        Seq.toString (fn s => "\n" ^ (Seq.toString e2s s)) cols

    type 'a matrix = 'a absmat

    fun size (M cols) = (Seq.length cols, Seq.length (Seq.nth 0 cols))
                          handle (Seq.Range _) => (0, 0)

    fun sub (M cols) (i, j) = Seq.nth j (Seq.nth i cols)

    fun repeat x (width, height) =
        M (Seq.repeat width (Seq.repeat height x))

    fun tabulate f (w, h) =
        M (Seq.tabulate (fn i => Seq.tabulate (fn j => f (i,j)) h) w)

    fun map f (M cols) = M (Seq.map (Seq.map f) cols)

    fun update (m as (M cols)) ((i, j), x) =
        let
          val (w, h) = size m

          fun update_one (s : 'a Seq.seq) (ind : int) (x : 'a) (k : int) =
              case ind = k of
                  true => x
                | false => Seq.nth k s
        in
          M (Seq.tabulate (update_one cols i
                             (Seq.tabulate
                                (update_one (Seq.nth i cols) j x) h)) w)
        end

    fun matching_subs (p : 'a -> bool) (m as (M cols)) =
        let
          val (w, h) = size m

          val cr_indexed =
              Seq.tabulate (fn i => Seq.tabulate
                                      (fn j => ((i,j), sub m (i, j)))
                                      h)
                           w

          val fst = fn (x, y) => x
          val snd = fn (x, y) => y
        in
          Seq.flatten (Seq.map
                         (fn c => Seq.map fst
                                          (Seq.filter (p o snd) c))
                         cr_indexed)
        end

    fun reduce (rowf: 'a * 'a -> 'a) (rowb : 'a)
               (colf: 'a * 'a -> 'a) (colb : 'a)
               (M cols : 'a matrix) =
          Seq.mapreduce (Seq.reduce colf colb) rowb rowf cols

    fun rows m =
        let
          val (w, h) = size m
        in
          Seq.tabulate (fn j => (Seq.tabulate (fn i => sub m (i, j)) w)) h
        end

    fun cols (M cols) = cols

    fun from_cols cols = M cols
    fun from_rows rows =
        let
            val iftheywerecols = M rows
            val (w, h) = size iftheywerecols
        in
            M (Seq.tabulate
                (fn i => Seq.tabulate
                    (fn j => sub iftheywerecols (j,i)) w) h)
        end


    fun diags1 m =
        let
          val (w, h) = size m
          val dmin = Int.min (w, h)
          val dmax = Int.max (w, h)
          val num_diags = w + h - 1

          fun take_diag (starti : int, k : int, len : int) =
              Seq.tabulate (fn i => sub m (i + starti, k - (i+starti))) len

          fun gen_diag (k : int) =
              case k < dmin of
                  true => take_diag (0, k, k + 1)
                | false =>
                  (case k >= dmax of
                       true =>
                           let
                             val num_elmts = num_diags - k
                           in
                             take_diag (w - num_elmts, k, num_elmts)
                           end
                     | false =>
                           (case w < h of
                                true => take_diag (0, k, dmin)
                              | false => take_diag (k - dmin + 1, k, dmin)
                                ))
        in
          Seq.tabulate gen_diag num_diags
        end

    fun diags2 m =
        let
          val (w, h) = size m
        in
          diags1 (M (Seq.tabulate (fn i => Seq.tabulate
                                        (fn j => sub m (i, h - j - 1)) h) w))
        end

    fun enum (m : 'a matrix) : ('a * (int*int)) matrix =
        tabulate (fn pos => (sub m pos, pos)) (size m)

    fun diags1From (m : 'a matrix) (col, row) : ('a*(int*int)) Seq.seq =
        Seq.nth (col + row) (diags1 (enum m))

    fun diags2From (m : 'a matrix) (col, row) : ('a*(int*int)) Seq.seq =
        let
          val (ncol,nrow) = size m
        in
          Seq.nth ((nrow-1) - row + col) (diags2 (enum m))
        end

    fun rowFrom (m : 'a matrix) (col, row) : ('a*(int * int)) Seq.seq =
        Seq.nth row (rows (enum m))

    fun colFrom (m : 'a matrix) (col, row) : ('a*(int * int)) Seq.seq =
        Seq.nth col (cols (enum m))


    fun between (m : 'a matrix) (scol,srow) (ecol,erow) : 'a Seq.seq =
        case (Int.compare (scol,ecol), Int.compare (srow,erow)) of
          (EQUAL, EQUAL) => Seq.empty ()
        | (EQUAL, _) =>
                   Seq.map (fn (x,pos) => x)
                   (Seq.filter (fn (_,(col,row)) => Int.min(srow,erow) < row
                                           andalso row < Int.max(srow,erow))
                   (colFrom m (scol,srow)))
        | (_, EQUAL) =>
                   Seq.map (fn (x,pos) => x)
                   (Seq.filter (fn (_,(col,row)) => Int.min(scol,ecol) < col
                                           andalso col < Int.max(scol,ecol))
                               (rowFrom m (scol,srow)))
        | (_, _) =>
          let
            val (numCol,numRow) = size m
          in
            case (scol+srow = ecol+erow,
                  (numRow - 1) - srow + scol = (numRow - 1) - erow + ecol) of
              (true, false) =>
                   Seq.map (fn (x,pos) => x)
                   (Seq.filter (fn (_,(col,row)) => Int.min(scol,ecol) < col
                                           andalso col < Int.max(scol,ecol))
                               (diags1From m (scol,srow)))
            | (false, true) =>
                   Seq.map (fn (x,pos) => x)
                   (Seq.filter (fn (_,(col,row)) => Int.min(scol,ecol) < col
                                           andalso col < Int.max(scol,ecol))
                               (diags2From m (scol,srow)))
            | (false, false) => raise Fail "No nice line between"
            | (true , true) => raise Fail "Int.compare is broken"
          end
end
