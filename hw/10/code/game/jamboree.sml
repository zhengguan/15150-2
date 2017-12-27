functor Jamboree (Settings : sig
                                 structure G : ESTGAME
                                 val search_depth : int
                                 val prune_percentage : real
                             end) : PLAYER =
struct

    exception NYI

    structure Game = Settings.G

    (* abbreviate Game as G, to keep the notation simple below. *)
    structure G = Game

    type edge  = G.move * G.est

    (* Implicit ordering:   NEGINF < Bound(v) < POSINF for all v *)
    datatype bound = NEGINF | Bound of G.est | POSINF
    type alphabeta = bound * bound    (* invariant: alpha < beta *)

    datatype orderAB = BELOW | INTERIOR | ABOVE

    structure Seq = BetterSeq(Seq)

    (* the following ToString functions my be helpful in testing *)
    fun valueToString v = "Value(" ^ G.Est.toString v ^ ")"

    fun edgeToString (m, v) = "Edge(" ^ G.move_to_string m ^ ", " ^ G.Est.toString v ^ ")"

    fun boundToString NEGINF = "NEGINF"
      | boundToString POSINF = "POSINF"
      | boundToString (Bound v) = "Bound(" ^ G.Est.toString v ^ ")"

    fun abToString (a,b) = "(" ^ boundToString a ^ "," ^ boundToString b ^ ")"

    (* lesseq : G.est * G.est -> bool *)
    fun lesseq(x, y) = (x = y) orelse
                       case (x, y) of
                         (G.Est.MinnieWins, _) => true
                       | (_, G.Est.MaxieWins) => true
                       | (G.Est.Guess n, G.Est.Guess m) => (n <= m)
                       | (_, _) => false

    (* compareAB : alphabeta -> G.est -> orderAB    *)
    (* REQUIRES: alphabeta inveriant: a < b         *)
    (* ENSURES:                                     *)
    (* compareAB (a,b) v  ==>                       *)
    (*                 BELOW      if  v <= a        *)
    (*                 INTERIOR   if  a < v < b     *)
    (*                 ABOVE      if  v >= b        *)

    (*  COPY this function from your alpha-beta implementation. *)
    fun compareAB ((a,b) : alphabeta) (v : G.est) : orderAB =
        case (a,b) of
          (NEGINF, Bound(v')) => (case lesseq(v',v) of
                                    true  => ABOVE
                                   |false => INTERIOR)
         |(Bound(v'), POSINF) => (case lesseq(v,v') of
                                    true  => BELOW
                                   |false => INTERIOR)
         |(Bound(v1), Bound(v2)) => (case (lesseq(v,v1),lesseq(v2,v)) of
                                       (true,_) => BELOW
                                      |(_,true) => ABOVE
                                      |(_,_) => INTERIOR)
         |(_,_) => INTERIOR

    (* maxEdge : edge option -> edge -> edge option                       *)
    (* REQUIRES: true                                                     *)
    (* ENSURES:  maxEdge e1op e2 returns SOME of the edge with max value. *)
    fun maxEdge NONE e = SOME(e)
      | maxEdge (SOME(m1,v1)) (m2,v2) = SOME(if lesseq(v2,v1) then (m1,v1) else (m2,v2))

    (* minEdge : edge option -> edge -> edge option                       *)
    (* REQUIRES: true                                                     *)
    (* ENSURES:  minEdge e1op e2 returns SOME of the edge with min value. *)
    fun minEdge NONE e = SOME(e)
      | minEdge (SOME(m1,v1)) (m2,v2) = SOME(if lesseq(v1,v2) then (m1,v1) else (m2,v2))

    (* bestMaxEdge : edge option -> edge option Seq.seq -> edge option       *)
    (* REQUIRES: true                                                        *)
    (* ENSURES: (bestMaxEdge eop s) returns SOME(edge) for edge with maximum *)
    (*          value in the sequence s@<eop> or NONE if no edge present.    *)
    (*                                                                       *)
    fun bestMaxEdge eop =
        let
           fun maxFn (NONE,opt) = opt
             | maxFn (opt,NONE) = opt
             | maxFn (SOME(m1,v1), SOME(m2,v2)) =
                if lesseq(v1,v2) then SOME(m2,v2) else SOME(m1,v1)
        in
           Seq.reduce maxFn eop
        end

    (* bestMinEdge : edge option -> edge option Seq.seq -> edge option       *)
    (* REQUIRES: true                                                        *)
    (* ENSURES: (bestMinEdge eop s) returns SOME(edge) for edge with minimum *)
    (*          value in the sequence s@<eop> or NONE if no edge present.    *)
    fun bestMinEdge eop =
        let
           fun minFn (NONE,opt) = opt
             | minFn (opt,NONE) = opt
             | minFn (SOME(m1,v1), SOME(m2,v2)) =
                if lesseq(v2,v1) then SOME(m2,v2) else SOME(m1,v1)
        in
           Seq.reduce minFn eop
        end

    (* splitMoves : G.state -> G.move Seq.seq * G.move Seq.seq
     * REQUIRES : true
     * ENSURES  : splits a sequence of moves according to the prune_percentage
     *            that should be abmoves and those that should be mmmoves      *)
    fun splitMoves (s : G.state) : ((G.move Seq.seq) * (G.move Seq.seq)) =
        let
           val moves = G.moves s
           val n = Seq.length moves
           val k = Real.floor(Settings.prune_percentage * (Real.fromInt n))
           val abmoves = Seq.tabulate (fn i => Seq.nth i moves) k
           val mmmoves = Seq.tabulate (fn i => Seq.nth (k + i) moves) (n-k)
        in
           (abmoves, mmmoves)
        end

    (* search : int -> alphabeta -> G.state -> edge option                   *)
    (* REQUIRES: d > 0, (G.moves s) is nonempty.                             *)
    (* ENSURES:  search d ab s ==> SOME(optimal outgoing edge from s),       *)
    (*           based on depth-d Jamboree,                                  *)
    (*           starting with alpha-beta interval "ab".                     *)
    (*           The percentage of moves searched with alpha-beta pruning    *)
    (*           is specified in Settings.                                   *)
    fun search (d : int) (ab : alphabeta) (s : G.state) : edge option =
        let
           val (abms, mmms) = splitMoves s
           val player = G.player s
        in
          (case player of
             G.Maxie  => maxisearch d ab s abms mmms NONE
            |G.Minnie => minisearch d ab s abms mmms NONE)
        end

    (* maxisearch : int -> alphabeta -> G.state -> G.move Seq.seq -> G.move Seq.seq -> edge option -> edge option *)
    (* REQUIRES: d > 0;                                                      *)
    (*           all moves should contain only moves that are legal at s;    *)
    (*           "s" is a Maxie state;                                       *)
    (*           "best" should not be NONE when there are no moves left.     *)
    (* ENSURES:  maxisearch d ab s abmoves mmmoves best                      *)
    (*                              ==> SOME(optimal outgoing edge from s),  *)
    (*                based on depth-d search,                               *)
    (*                first using alpha-beta pruning over "abmoves",         *)
    (*                then using minimax over "mmoves".                      *)
    (*                "ab" and "best" are accumulator arguments for the      *)
    (*                current alpha-beta interval and current best edge.     *)
    and maxisearch (d : int) (ab as (a, b)) (s : G.state)
                   (abmoves : G.move Seq.seq)(mmmoves : G.move Seq.seq)
                   (best : edge option) : edge option =
        case (Seq.showl abmoves) of
          Seq.Nil =>
            (case (Seq.showl mmmoves) of
               Seq.Nil => best
              |_ => bestMaxEdge best (Seq.map (fn mv =>
                     SOME(mv, (evaluate (d-1) ab (G.make_move(s,mv)))))
                                              mmmoves))
         |Seq.Cons(m,ms) =>
          (let
              val v = evaluate (d-1) ab (G.make_move(s,m))
              val best' = maxEdge best (m,v)
           in
              (case (compareAB ab v) of
                 BELOW => maxisearch d ab s ms mmmoves best'
                |INTERIOR => maxisearch d (Bound(v),b) s ms mmmoves best'
                |ABOVE => best')
           end)

    (* minisearch : int -> alphabeta -> G.state -> G.move Seq.seq -> G.move Seq.seq -> edge option -> edge option *)
    (* REQUIRES: d > 0;                                                      *)
    (*           all moves should contain only moves that are legal at s;    *)
    (*           "s" is a Minnie state;                                      *)
    (*           "best" should not be NONE when there are no moves left.     *)
    (* ENSURES:  minisearch d ab s abmoves mmmoves best                      *)
    (*                              ==> SOME(optimal outgoing edge from s),  *)
    (*                based on depth-d search,                               *)
    (*                first using alpha-beta pruning over "abmoves",         *)
    (*                then using minimax over "mmoves".                      *)
    (*                "ab" and "best" are accumulator arguments for the      *)
    (*                current alpha-beta interval and current best edge.     *)
    and minisearch (d : int) (ab as (a, b)) (s : G.state)
                   (abmoves : G.move Seq.seq)(mmmoves : G.move Seq.seq)
                   (best : edge option) : edge option =
        case (Seq.showl abmoves) of
          Seq.Nil =>
            (case (Seq.showl mmmoves) of
               Seq.Nil => best
              |_ => bestMinEdge best (Seq.map (fn mv =>
                        SOME(mv,(evaluate (d-1) ab (G.make_move(s,mv)))))
                                              mmmoves))
         |Seq.Cons(m,ms) =>
          (let
              val v = evaluate (d-1) ab (G.make_move(s,m))
              val best' = minEdge best (m,v)
           in
              (case (compareAB ab v) of
                 BELOW => best'
                |INTERIOR => minisearch d (a,Bound(v)) s ms mmmoves best'
                |ABOVE => minisearch d ab s ms mmmoves best')
           end)

    (* evaluate : int -> alphabeta -> G.state -> G.est                     *)
    (* REQUIRES: d >= 0                                                    *)
    (* ENSURES:  evaluate d ab s ==> value attributed to state s, based on *)
    (*                               depth-d Jamboree search.              *)
    and evaluate (d : int) (ab : alphabeta) (s : G.state) : G.est =
        case d of
          0 => G.estimate s
         |_ => (case (Seq.length (G.moves s)) of
                  0 => G.estimate s
                 |_ => (case (search d ab s) of
                          SOME((_,v)) => v
                         |NONE => raise Fail "It wasn't supposed to end like this..."))

    (* recall:  the signature requires that s be In_play. *)
    (* next_move : G.state -> G.move
     * REQUIRES: s is In_play
     * ENSURES: next_move s => the best move outgoing from s, found using
     *          Jamboree search and the constants specified in Settings
     *)
    fun next_move (s : G.state) : G.move =
        let
           val d = Settings.search_depth
           val SOME (m,_) = search d (NEGINF,POSINF) s
        in
           m
        end

end (* Jamboree *)
