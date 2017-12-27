functor TicTacToe(Settings : TTTCONSTS) : ESTGAME =
struct

exception Unimplemented

(* Make our sequences better! *)
structure Seq = BetterSeq(Seq)
open Seq

datatype player = Minnie | Maxie
datatype outcome = Winner of player
datatype status = Over of outcome | In_play

(* NONE means no entry yet, SOME(e) means a tile has entry e *)
exception IllegalMove
datatype entry = O | X
type tile = entry option
type board = tile seq seq
type state = board * player
type location = int * int
type move = entry * location

structure Est : EST = Estimate
type est = Est.est

(* The start state is an n x n board of tiles. *)
(* Here, n = Settings.board_size, so each row and col has length
                                                 Settings.board_size*)
(* The first player to make a move will always be Maxie (he is a mean guy) *)
val n = Settings.board_size

val start : state = ((tabulate (fn i =>
                    (tabulate (fn j => NONE) n)) n), Maxie)

(* Don't worry about this one *)
fun data_to_start_state _ = start

val move_eq = (op =)

(* Might be a useful function... *)
fun other_player (p : player) =
    case p of
      Maxie => Minnie
    | Minnie => Maxie

(* is_valid_move : (state * move) -> bool
 * REQUIRES: s is a valid state (i.e. square board, correct player, etc.
 * ENSURES: is_valid_move (s,m) ==> true if m described a move
 *          within the bounds of the board, and where the location
 *          given by m is not already taken by another entry, and
 *                              ==> false otherwise
 *)
fun is_valid_move (s : state, m : move) : bool =
   let
      val (b,_) = s
      val (_, (r,c)) = m
      val n = length b
      (* remember, s is a square board! n = # rows and # cols*)
   in
     case ((r > (n-1)), (c > (n-1))) of
       (true,_) => false
      |(_,true) => false
       (* board is 0-indexed, so greatest possible move index is (n-1)*)
      |(_,_) => (case (nth c (nth r b)) of
                   NONE => true
                  |_ => false)
   end

val true = is_valid_move(start,(X,(0,0)))
val false = is_valid_move(start,(O,(n,n)))

(* make_move : state * move -> state
 * REQUIRES: m is a valid move in s
 * ENSURES: make_move (s,m) => s', a state identical to s except that
 *          the tile at the index specified by m is replaced with
 *          the entry specified by m
 *)
fun make_move (s : state, m : move) : state =
   let
      val (b,p) = s
      val (entr, (i, j)) = m
      val b' = insert (insert (SOME(entr)) (nth i b) j) b i
   in
      (b', (other_player p))
   end

(* moves : state -> move seq
 * REQUIRES: true
 * ENSURES: moves s => a sqequences of possible valid moves given
 *          the board and player in s
 *)
fun moves (s : state) : move seq =
   let
      val (b,p) = s
      val entr = case p of Minnie => X | Maxie => O
      val idtiles = mapIdx (fn (row : tile seq, i : int) =>
                    mapIdx (fn (t : tile, j : int) => (t, (i,j))) row) b
   in
      map (fn (_,(i,j)) => (entr,(i,j))) (filter (fn (NONE,(_,_)) => true
                                                    |_ => false)
                                                 (flatten idtiles))
   end

(* entryEq : entry * entry -> bool
 * REQUIRES: true
 * ENSURES: entryEq(e1, e2) => true if e1 = e2, and
                            => false otherwise
 *)
fun entryEq (e1 : entry, e2 : entry) : bool =
    case (e1, e2) of
      (O, O) => true
     |(X, X) => true
     |(_, _) => false

(* checkSeq : tile seq -> entry option
 * REQUIRES: (length tSeq = Settings.board_size
 * ENSURES: checkSeq tSeq => SOME entr if tSeq consists solely of
 *          the same entries, with no NONEs or the other type of entry, and
 *                        => NONE otherwise
 *)
fun checkSeq (tSeq : tile seq) : entry option =
    let
       val potentialSeq = extractSomes tSeq
       val l = length potentialSeq
    in
       case (l = 0) of
         true => NONE
        |false => (let
                      val initEntr = (nth 0 potentialSeq)
                      val potentialSeq' = (filter (fn entr =>
                                                     entryEq(initEntr, entr))
                                          potentialSeq)
                   in
                      (case ((length potentialSeq') = Settings.board_size) of
                         true => (SOME initEntr)
                        |false => NONE)
                   end)
    end

val test1 = tabulate (fn i => SOME(X)) Settings.board_size
val test2 = tabulate (fn i => (case i of
                                 0 => SOME(X)
                                |_ => NONE)) Settings.board_size
val test3 = tabulate (fn i => (case i of
                                 0 => SOME(X)
                                |1 => SOME(O)
                                |_ => SOME(X))) Settings.board_size
val SOME(X) = checkSeq test1
val NONE = checkSeq test2
val NONE = checkSeq test3

(* checkRows : board -> entry option
 * REQUIRES: only one winning row exists in b, if one exists
 * ENSURES: checkRows b => SOME entr if there is a row in b that is filled
 *          with only (SOME entr), and
 *                      => NONE otherwise
 *)
fun checkRows (b : board) : entry option =
    let
       val resultSeq = (filter (fn NONE => false
                                 |_ => true) (map checkSeq b))
    in
       case (length resultSeq) of
         0 => NONE
        |_ => nth 0 resultSeq
    end

(*invert : board -> board
 * REQUIRES: ss is an n by n 2D sequence
 * ENSURES: invert ss => ss', where the rows and columns of ss are switched
 *)
fun invert (ss : 'a seq seq) : 'a seq seq =
    tabulate (fn i => tabulate (fn j => nth i (nth j ss)) n) n

(* checkCols : board -> entry option
 * REQUIRES: only one winning column exists in b, if one exists
 * ENSURES: checkCols b => SOME entr if there is a column in b that is filled
 *          with only (SOME entr), and
 *                      => NONE otherwise
 *)
fun checkCols (b : board) : entry option =
       checkRows (invert b)

(* checkDiags : board -> entry option
 * REQUIRES: if both diagonals are winning diagonals, they are for the
 *           same player
 * ENSURES: checkDiags b => SOME entr if at least one of the diagonals in b
 *          is filled with only (SOME entr), and
 *                       => NONE otherwise
 *)
fun checkDiags (b : board) : entry option =
    let
       val diag1 = tabulate (fn i => nth i (nth i b)) n
       val diag2 = tabulate (fn i => nth (n - 1 -i) (nth i b)) n
    in
       case ((checkSeq diag1), (checkSeq diag2)) of
         ((SOME entr),_) => SOME entr
        |(_,(SOME entr)) => SOME entr
        |_ => NONE
    end

(* entryToPlayer : entry -> player
 * REQUIRES: true
 * ENSURES: entryToPlayer entr => Minnie if entr = X, and
 *                             => Maxie if entr = O
 *)
fun entryToPlayer (entr : entry) : player =
         case entr of
           X => Minnie
          |O => Maxie

val Minnie = entryToPlayer X
val Maxie = entryToPlayer O

(* checkFull : board -> bool
 * REQUIRES: b is a valid n by n board with NO winning rows/columns/diagonals
 * ENSURES: checkFull b => true if the board is full, and
                        => false otherwise
 *)
fun checkFull (b : board) : bool =
    let
       val entrBoard = map extractSomes b
       val fullRows = filter (fn row : entry seq =>
                                 ((length row) = n)) entrBoard
    in
       ((length fullRows) = n)
    end

(* status : state -> status
 * REQUIRES: s is a valid state, and if more than one
             winning row/column/diagonal exists, they are all for the same
 *           player
 * ENSURES: status s => Over(Winner(p')) if p' has filled all of a
 *          row, column, or diagonal with the corresponding entry,
 *                   => Over(Winner(Minnie)) if the board is full with no
 *          winnings rows, columns, or diagonals, and
 *                   => In_play otherwise
 *)
fun status (s as (b, p) : state) : status =
    case ((checkRows b), (checkCols b), (checkDiags b)) of
      (NONE, NONE, NONE) => (case (checkFull b) of
                  true  => Over(Winner(Minnie))
                 |false => In_play)
     |((SOME entr),_,_) => Over(Winner(entryToPlayer entr))
     |(_,(SOME entr),_) => Over(Winner(entryToPlayer entr))
     |(_,_,(SOME entr)) => Over(Winner(entryToPlayer entr))


fun player ((_, p) : state) : player = p

fun outcome_to_est (Winner(Maxie)) = Est.MaxieWins
  | outcome_to_est (Winner(Minnie)) = Est.MinnieWins

fun outcome_to_string (Winner(Maxie)) = "Maxie wins"
  | outcome_to_string (Winner(Minnie)) = "Minnie wins"

fun player_to_string (Maxie : player) : string = "Maxie"
  | player_to_string Minnie = "Minnie"

(* consecSeqs : int seq -> int seq seq
 * REQUIRES: s is a sequence of either 0, 1, or ~1
 * ENSURES: consecSeqs s => a sequence of int sequences representing
 *          the sequence of consecutive 1s, 0s, or ~1s as they appear in
 *          reverse order in s
 *)
fun consecSeqs (s : int seq) : int seq seq =
    mapreduce (fn i => singleton(singleton(i))) (empty ())
              (fn (ss1 : int seq seq, ss2 : int seq seq) =>
                  (case ((nth 0 (nth 0 ss1)) = (nth 0 (nth 0 ss2))) of
                    true  => update(ss1, 0, (append((nth 0 ss1),(nth 0 ss2))))
                   |false => append(ss2, ss1)) handle (Range _) => ss2) s

val seq1 = fromList [0,0,0,1,1,~1,~1,~1,0,0]
val list2 = [(fromList [0,0]),
                     (fromList [~1,~1,~1]),
                     (fromList [1,1]),
                     (fromList [0,0,0])]
val seq2 = fromList list2
val list2 = toList (consecSeqs seq1)

(* greatestConsec : int seq seq -> int
 * REQUIRES: ss is a sequence of int sequences of consecutive 0s, 1s, or ~1s
 * ENSURES: greatestConsec ss => 1 or ~1 if the longest sequence of
 *          consecutive integers in ss is of 1s or ~1s respectively, and
 *                            => 0 if there is a tie between 1s and ~1s
 *)
fun greatestConsec (ss : int seq seq) : int =
    let
       val noZeroes = filter (fn iSeq : int seq => (case (nth 0 iSeq) of
                                                      0 => false
                                                     |_ => true)) ss
       val (winner, count) = mapreduce (fn iSeq : int seq =>
                                           ((nth 0 iSeq) : int,
                                            (length iSeq) : int)) (0,0)
                             (fn ((e1,l1),(e2,l2)) =>
                                 (case ((e1 = e2), (Int.compare(l1,l2))) of
                                    (true, EQUAL) => (e1, l1)
                                   |(_,LESS) => (e2, l2)
                                   |(_,GREATER) => (e1, l1)
                                   |(_,_) => (0,0))) noZeroes
    in
      winner
    end

val ~1 = greatestConsec seq2

(* estimate : state -> est
 * REQUIRES: (b, p) is a valid game state
 * ENSURES: estimate (b,p) => Est.MinnieWins if there is a complete
 *                            row/column/diagonal filled with Xs,
 *                         => Est.MaxieWins if there is a complete
 *                            row/column/diagonal filled with Os, and
 *                         => Est.Guess x otherwise, where x is a
 *                            numerical representation of how good that
 *                            game state is for Minni or Maxie. A lower x
 *                            is better for Minnie, a higher x is better for
 *                            Maxie.
 *)
fun estimate ((b, p) : state) =
    case (status (b,p)) of
      Over(Winner(Minnie)) => Est.MinnieWins
     |Over(Winner(Maxie)) => Est.MaxieWins
     |In_play => (let
                     fun playerMult (p' : player) = case p' of Minnie => ~1
                                                              |Maxie => 1
                     val n = Settings.board_size
                     val scoreSeq =
                         map (fn row : tile seq =>
                         map (fn t : tile =>
                                 case t of
                                   NONE => 0
                                 |SOME entr => playerMult(entryToPlayer entr))
                              row) b
                     val weighted = mapIdx (fn (row : int seq, i : int) =>
                                    mapIdx (fn (score : int, j : int) =>
                       case ((i = 0),(j = 0), (i = (n - 1)), (j = (n - 1))) of
                              (true, true,_,_) => 3 * score
                             |(_,_,true,true) => 3 * score
                             |(true,_,_,true) => 3 * score
                             |(_,true,true,_) => 3 * score
                             |_ =>
                        (case ((i=j), i = (n-1-j)) of
                           (true,_) =>
                              (case (((n mod 2) = 1) andalso
                                     (i = (n div 2))) of
                                 true  => 3 * score
                                |false => 2 * score)
                          |(_,true) => 2 * score
                          |_ => score)) row) scoreSeq
                     val entryScore = reduce op+ 0 (flatten weighted)
                     val rowConsecBonus = mapreduce
                                 (greatestConsec o consecSeqs) 0 op+ scoreSeq
                     val colConsecBonus = mapreduce
                                 (greatestConsec o consecSeqs) 0 op+
                                          (invert scoreSeq)
                     val diagConsecBonus1 = (greatestConsec o consecSeqs)
                                 (tabulate (fn i =>
                                               nth i (nth i scoreSeq)) n)
                     val diagConsecBonus2 = (greatestConsec o consecSeqs)
                                 (tabulate (fn i =>
                                               nth i (nth (n-1-i) scoreSeq)) n)
                  in
                    Est.Guess((entryScore + rowConsecBonus
                                          + colConsecBonus
                                          + diagConsecBonus1
                                          + diagConsecBonus2))
                  end)

(* The score is estimated like so:
 * 1. Every tile is converted into a number: NONE is 0, SOME X is ~1 for
 *    Minnie, SOME O is 1 for Maxie, giving a new "scoreSeq" sequence
 * 2. Numbers that were in corner tiles are weighted 3x, and
 *    numbers that were in the non-corner diagonals are weighted 2x, giving
 *    a new "weighted" score sequence
 * 3. This new "weighted" score sequence is flattened and then summed to give
 *    an initial "entryScore" estimate
 * 4. Then, using the helper functions greatestConsec and consecSeqs,
 *    +1 or -1 points are awarded for each row, column, and complete diagonal
 *    where either Maxie or Minnie has the most consecutive entries. If there
 *    is a tie, 0 points are awarded
 * 5. The final estimate is "entryScore" with all of the bonuses added
 *)

(* The given functions below are for playing the game - no need to worry about these *)
(* That being said, DON'T CHANGE THEM *)
(* Colors for to_string methods *)
val color_O = Ansi.bright_cyan
val color_X = Ansi.bright_magenta

fun entry_to_string (O : entry) : string = Ansi.colorStr "O" {bg = NONE, fg = SOME(color_O)}
  | entry_to_string X = Ansi.colorStr "X" {bg = NONE, fg = SOME(color_X)}

fun detailed_move_to_string ((b,p) : state, (e,(r,c)) : move) =
    (player_to_string p) ^ " placed an " ^
    (entry_to_string e) ^ " onto row " ^
    Int.toString(r) ^ " and column " ^
    Int.toString(c)

fun move_to_string ((e, (r,c)) : move) : string =
    "Placed " ^ (entry_to_string e) ^
    " on location (" ^ Int.toString(r) ^ ", " ^ Int.toString(c) ^ ")"

val turn_color = Ansi.bright_yellow
fun state_to_string ((b, p) : state) =
    let
      val divider = String.concat (List.tabulate (4 * Settings.board_size + 1,(fn _ => "-")))
      fun eopt_to_string eo = case eo of
                                SOME(O) => " O "
                              | SOME(X) => " X "
                              | NONE => " - "
      fun row_to_string (s : entry option seq) =
          "|" ^ String.concatWith "|" (Seq.toList(Seq.map eopt_to_string s)) ^ "|"
      val rowStrings = Seq.map row_to_string b
      val boardString = String.concatWith ("\n" ^ divider ^ "\n") (Seq.toList rowStrings)
      val nextMoveString = Ansi.colorStr ((player_to_string p) ^ " to move. Board State:")
                                         {bg = NONE, fg = SOME(turn_color)}
    in
      nextMoveString ^ "\n\n" ^ divider ^ "\n" ^ boardString ^ "\n" ^ divider ^ "\n"
    end

(* Maintains the invariant as given in the signature *)
fun move_to_descriptor ((_,(r,c)) : move) = Int.toString(r) ^ " " ^ Int.toString(c)

fun parse_move ((b, p) : state) (str : string) : move option =
    let
      val tokens = String.tokens (fn x => x = #" ") str
    in
      case tokens of
        [r,c] => (case (Int.fromString(r), Int.fromString(c)) of
                    (SOME(row),SOME(col)) => if p = Maxie
                                             then SOME(O, (row, col))
                                             else SOME(X, (row, col))
                  | _ => NONE)
      | _ => NONE
    end

end
