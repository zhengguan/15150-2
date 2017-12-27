structure SetNoDuplicates : INTSET =
struct

  type set = int list

  val empty = fn () => []

  fun find (n : int) ([] : set) = false
    | find n (x :: l) = (x = n) orelse find n l

  (* No Duplicates allowed *)
  fun insert (n : int) (s : set) = 
      if find n s then s else n :: s

  (* you must delete all duplicates in this *)
  (* also should handle the case in which n is not in the set *)
  fun delete (n : int) (s : set) = 
      List.filter (fn x => not (x = n)) s

  (* union is not allowed to have any duplicates in it *)
  fun union (s1 : set) (s2 : set) = 
      s1 @ (List.filter (fn x => not (find x s1)) s2)

  (* all elements in s1 and s2 *)
  fun intersection (s1 : set) (s2 : set) = 
      List.filter (fn x => find x s2) s1

  (* s1 - s2 *)
  (* list.partition is helpful *)
  fun difference (s1 : set) (s2 : set) = 
      List.filter (fn x => not (find x s2)) s1

end 

structure TestSND = 
struct
  val true = SetNoDuplicates.find 1 [1,2,3]
  val [1,2] = SetNoDuplicates.insert 1 [1,2]
  val [2] = SetNoDuplicates.delete 1 [1,2]
  val [1,2,3,4] = SetNoDuplicates.union [1,2] [1,3,4]
  val [1,2] = SetNoDuplicates.intersection [1,2,3,4] [5,6,1,2]
  val [1,2] = SetNoDuplicates.difference [1,2,3,4] [3,4] 
end
