(* Example CONST structures can be found in
 * lib/game/options.sml
 *)

structure GameData =
struct

(* POKEMON(name, type1, type2, HP, ATT, DEF, SPA, SPD, move_id_list) *)
datatype game_data = POKEMON of string * int option * int option *
                           int * int * int * int * int *
                           int list

(* Defines how to generate a code for each game *)
fun entry_to_string (POKEMON(name,t1,t2,hp,att,def,spa,spd,moves) : game_data) =
    let
      val sanitized_name = if name = ""
                           then ""
                           else List.nth ((String.tokens (fn c => c = #"|" orelse c = #"*") name), 0)
      val t1str = case t1 of
                    NONE => "~1"
                  | SOME(e1) => if e1 < 0 orelse e1 > 17
                                then raise Fail "Invalid Type for Pokemon Entry Data supplied"
                                else Int.toString e1
      val t2str = case t2 of
                    NONE => "~1"
                  | SOME(e2) => if e2 < 0 orelse e2 > 17
                                then raise Fail "Invalid Type for Pokemon Entry Data supplied"
                                else Int.toString e2
    in
      sanitized_name ^ "|" ^ t1str ^ "|" ^ t2str ^ "|" ^
      Int.toString(hp) ^ "|" ^ Int.toString(att) ^ "|" ^
      Int.toString(def) ^ "|" ^ Int.toString(spa) ^ "|" ^
      Int.toString(spd) ^ "|" ^
      (String.concatWith "|" (List.map (Int.toString) moves))
    end

fun data_to_string (L : game_data list) =
    String.concatWith "*" (List.map entry_to_string L)

fun string_to_entry (entry : string) : game_data =
    let
      val entry_data = String.tokens (fn c => c = #"|") entry
      val name::t1str::t2str::hpstr::attstr::defstr::spastr::spdstr::move_ids = entry_data
          handle _ => raise Fail "Pokemon Entry Data could not be parsed"
      val (SOME(t1i),SOME(t2i)) = (Int.fromString t1str, Int.fromString t2str)
      val SOME(t1) = if t1i < 0 orelse t1i > 17
                     then raise Fail "Invalid Type for Pokemon Entry Data given"
                     else SOME(t1i)
      val t2o = if t2i < 0 orelse t2i > 17
                then NONE
                else SOME(t2i)
      val [hp,att,def,spa,spd] =
          List.map (fn SOME(i) => if i <= 0
                                  then raise Fail "Cannot have negative stats"
                                  else i
                     | NONE => raise Fail "Invalid given stats")
                   (List.map (Int.fromString) [hpstr,attstr,defstr,spastr,spdstr])
      val moves = List.map (fn SOME(i) => i | NONE => raise Fail "Invalid given move ID's")
                           (List.map (Int.fromString) move_ids)
    in
      POKEMON(name,SOME(t1),t2o,hp,att,def,spa,spd,moves)
    end

fun string_to_data (data : string) : game_data list =
    List.map string_to_entry (String.tokens (fn c => c = #"*") data)

end

signature CONSTS =
sig

(* Construction of the Pokemon to use *)
val maxie_data : GameData.game_data list
val minnie_data : GameData.game_data list

(* The level of all the Pokemon
 * which will affect damage calculation.
 * By convention, this should be 50.
 *)
val level : int

(* The amount of damage done to a pokemon
 * due to fatigue.
 * By convention, this should be 1.
 *)
val fatigue_damage : int

end

signature TTTCONSTS =
sig

(* The dimensions of the board *)
val board_size : int

end
