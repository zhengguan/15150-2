signature CLIENTDATA =
sig

  (* username when connecting to the server *)
  val username : string

  (* team number - for matching *)
  val team : int

  (* identification number *)
  val identification : int

end
