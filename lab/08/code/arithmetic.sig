signature ARITHMETIC =
sig

  type integer

  (* converts type int into the specified integer type *)
  val rep : int -> integer

  (* converts type integer to int *)
  val toInt : integer -> int

  (* allows you to view the integer as a string *)
  val display : integer -> string

  (* add two integers together *)
  val add : integer * integer -> integer

  (* multiply two integers together *)
  val mult : integer * integer -> integer

end


