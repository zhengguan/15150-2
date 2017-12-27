signature SEARCH =
sig
  structure P : SEARCHABLE
  val find_help : P.elem -> P.stree ->
                  (P.result -> 'a) -> (unit -> 'a) -> 'a
  val find : P.elem -> P.stree -> P.result option
end
