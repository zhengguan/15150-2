signature SETTINGS =
sig
    structure G : ESTGAME
    val search_depth : int
end

functor AlphaBeta (Settings : SETTINGS) : PLAYER =
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

    (* the following ToString functions my be helpful in testing *)
    fun valueToString v = "Value(" ^ G.Est.toString v ^ ")"

    fun edgeToString (m, v) = "Edge(" ^ G.move_to_string m ^ ", " ^ G.Est.toString v ^ ")"

    fun boundToString NEGINF = "NEGINF"
      | boundToString POSINF = "POSINF"
      | boundToString (Bound v) = "Bound(" ^ G.Est.toString v ^ ")"

    fun abToString (a,b) = "(" ^ boundToString a ^ "," ^ boundToString b ^ ")"

    (* lesseq : G.est * G.est -> bool *)
    fun lesseq(x, y) = (x=y) orelse
         case (x, y) of
              (G.Est.MinnieWins, _) => true
            | (_, G.Est.MaxieWins) => true
            | (G.Est.Guess n, G.Est.Guess m) => (n <= m)
            | (_, _) => false

    structure Seq = BetterSeq(Seq)

    (* compareAB : alphabeta -> G.est -> orderAB    *)
    (* REQUIRES: alphabeta invariant: a < b         *)
    (* ENSURES:                                     *)
    (* compareAB (a,b) v  ==>                       *)
    (*                 BELOW      if  v <= a        *)
    (*                 INTERIOR   if  a < v < b     *)
    (*                 ABOVE      if  v >= b        *)
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


    (* search : int -> alphabeta -> G.state -> edge option               *)
    (* REQUIRES: d > 0, (G.moves s) is nonempty.                         *)
    (* ENSURES:  search d ab s ==> SOME(optimal outgoing edge from s),   *)
    (*           based on depth-d alpha-beta prunings,                   *)
    (*           starting from alpha-beta interval "ab",                 *)
    (*                         ==> NONE if the move sequence is empty    *)
    (*                                                                   *)
    (* search uses helper functions maxisearch and minisearch to perform *)
    (* the actual search, including updating the alpha-beta interval     *)
    (* and the best edge seen so far, as well as any possible pruning.   *)
    fun search (d : int) (ab : alphabeta) (s : G.state) : edge option =
        let
           val ms = G.moves s
           val player = G.player s
        in
           case player of
             G.Maxie => maxisearch d ab s ms NONE
            |G.Minnie => minisearch d ab s ms NONE
        end

    (* maxisearch : int -> alphabeta -> G.state -> G.move Seq.seq -> edge option -> edge option *)
    (* REQUIRES: d > 0; "moves" should contain only moves that are legal at s;         *)
    (*           "s" is a Maxie state;                                                 *)
    (*           "best" should not be NONE when "moves" is Nil.                        *)
    (* ENSURES:  maxisearch d ab s moves best ==> SOME(optimal outgoing edge from s),  *)
    (*           based on depth-d alpha-beta pruning over "moves",                     *)
    (*           starting from alpha-beta interval "ab", with accumulator              *)
    (*           "best" as default if no better edge is found.                         *)
    and maxisearch (d : int) (ab as (a,b)) (s : G.state)
                   (moves : G.move Seq.seq)
                   (best : edge option) : edge option =
        case (Seq.showl moves) of
              Seq.Nil => best
             |Seq.Cons(m,ms) =>
              (let
                  val v = evaluate (d-1) ab (G.make_move(s,m))
                  val best' = maxEdge best (m,v)
               in
                 (case (compareAB ab v) of
                    BELOW => maxisearch d ab s ms best'
                   |INTERIOR => maxisearch d (Bound(v),b) s ms best'
                   |ABOVE => best')
               end)
    (* minisearch : int -> alphabeta -> G.state -> G.move Seq.seq -> edge option -> edge option *)
    (* REQUIRES: d > 0; "moves" should contain only moves that are legal at s;         *)
    (*           "s" is a Minnie state;                                                *)
    (*           "best" should not be NONE when "moves" is Nil.                        *)
    (* ENSURES:  minisearch d ab s moves best ==> SOME(optimal outgoing edge from s),  *)
    (*           based on depth-d alpha-beta pruning over "moves",                     *)
    (*           starting from alpha-beta interval "ab", with accumulator              *)
    (*           "best" as default if no better edge is found.                         *)
    and minisearch (d : int) (ab as (a,b)) (s : G.state)
                   (moves : G.move Seq.seq)
                   (best : edge option) : edge option =
        case (Seq.showl moves) of
              Seq.Nil => best
             |Seq.Cons(m,ms) =>
              (let
                  val v = evaluate (d-1) ab (G.make_move(s,m))
                  val best' = minEdge best (m,v)
               in
                 (case (compareAB ab v) of
                    BELOW => best'
                   |INTERIOR => minisearch d (a,Bound(v)) s ms best'
                   |ABOVE => minisearch d ab s ms best')
               end)

    (* evaluate : int -> alphabeta -> G.state -> G.est                     *)
    (* REQUIRES: d >= 0                                                    *)
    (* ENSURES:  evaluate d ab s ==> value attributed to state s, based on *)
    (*                               depth-d alpha-beta search.            *)
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
     * ENSURES: next_move s => the best move outgoing from s
     *)
    fun next_move (s : G.state) : G.move =
        let
           val d = Settings.search_depth
           val SOME(m,_) = search d (NEGINF,POSINF) s
        in
          m
        end

end (* AlphaBeta *)
