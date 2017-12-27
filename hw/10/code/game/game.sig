signature EST =
sig
    datatype est = MinnieWins | Guess of int | MaxieWins

    (* Compares two estimated values and returns an order *)
    val compare : (est * est) -> order

    (* Utility function for printing estimated values *)
    val toString : est -> string
end

signature GAME =
sig

    type state (* state of the game; e.g. board and player *)
    type move (* moves *)

    (* starting state *)
    val data_to_start_state : string list -> state
    val start : state

    (* REQUIRES:  m is in moves(s)                 *)
    (* ENSURES:   make_move(s,m) returns a value   *)
    val make_move : state * move -> state

    (* REQUIRES: status(s) = In_play                                       *)
    (* ENSURES:  moves(s) returns a nonempty sequence of moves legal at s. *)
    val moves : state -> move Seq.seq

    (* Simple functions that can be used for external UI, etc. *)
    val detailed_move_to_string : state * move -> string
    val move_to_string : move -> string
    val state_to_string : state -> string
    val move_eq : move * move -> bool
    val is_valid_move : state * move -> bool

    (* REQUIRES: string is single line and *not* terminated by newline.        *)
    (* ENSURES:  move described by string is legal at state, else return NONE. *)
    (* ENSURES:  given state s, parse_move s (move_to_descriptor move) = move. *)
    val move_to_descriptor : move -> string
    val parse_move : state -> string -> move option

end

signature ESTGAME =
sig
  structure Est : EST
  type est = Est.est


  datatype player = Minnie | Maxie
  datatype outcome = Winner of player
  datatype status = Over of outcome | In_play

  include GAME

  val estimate : state -> est

  (* A function for converting wins and draws to estimator values *)
  val outcome_to_est : outcome -> est

  val status : state -> status
  val player : state -> player
  val outcome_to_string : outcome -> string
  val player_to_string : player -> string
end
