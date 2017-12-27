(*
Game Client signature for the 15150 Game.
This utilizes the SocketUtil structure to
connect to the main server and play a match
with an opponent.
*)

signature GCLIENT =
sig

  (* connectToServer : string * int -> unit option *)
  (* REQUIRES: true *)
  (* ENSURES: connectToServer(host,port) returns SOME(())
   * if a connection is successful to the server,
   * and NONE otherwise *)
  val connectToServer : string * int -> unit option

end
