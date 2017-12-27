(* Duplicates are not permitted in any function *)
structure SetNoDuplicates : INTSET =
struct
  exception NYI

  type set = int list
  val empty = (fn () => raise NYI)

  fun find (n : int) (s : set) = raise NYI

  fun insert (n : int) (s : set) = raise NYI

  fun delete (n : int) (s : set) = raise NYI

  fun union (s1 : set) (s2 : set) = raise NYI

  fun intersection (s1 : set) (s2 : set) = raise NYI

  fun difference (s1 : set) (s2 : set) = raise NYI

end

structure TestSetNoDuplicates =
struct

end
