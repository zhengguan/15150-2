structure Estimate : EST =
struct
  datatype est = Guess of int | MinnieWins | MaxieWins

  fun compare (e1,e2) =
        if e1 = e2 then EQUAL
        else
          (case (e1,e2) of
               (MaxieWins,_) => GREATER
             | (MinnieWins,_) => LESS
             | (_,MaxieWins) => LESS
             | (_,MinnieWins) => GREATER
             | (Guess v1, Guess v2) => Int.compare(v1,v2))

  fun toString(Guess n) = "Guess:" ^ Int.toString n
    | toString MinnieWins = "Minnie wins!"
    | toString MaxieWins = "Maxie wins!"
end
