signature SOCKETUTIL =
sig

  type socket = Socket.active Socket.stream INetSock.sock

  (* connectTo : string * int -> socket option *)
  (* REQUIRES: true *)
  (* ENSURES: connectTo(host,port) returns SOME(s), where
   * is a s socket that is connected to the given
   * host and port number, if a connection is successful,
   * and NONE otherwise *)
  val connectTo : string * int -> socket option

  (* write : socket -> string -> unit option *)
  (* REQUIRES: true *)
  (* ENSURES: write s str returns SOME(()) if the
   * socket write is successful and NONE otherwise *)
  val write : socket -> string -> unit option

  (* read : socket -> int -> Time.time option -> string option *)
  (* REQUIRES: n > 0 *)
  (* ENSURES: read s n t returns SOME(str),
   * where str is the string returned by reading
   * from the socket s if successful within time limit t;
   * otherwise, NONE is returned *)
  val read : socket -> int -> Time.time option -> string option

  (* close : socket -> unit *)
  (* REQUIRES: s is not closed *)
  (* ENSURES: close s closes the socket s *)
  val close : socket -> unit

end
