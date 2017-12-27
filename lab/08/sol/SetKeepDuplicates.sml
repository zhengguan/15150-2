structure SetKeepDuplicates : INTSET =
struct

  type set = int list

  val empty = fn () => []

  fun find (n : int) ([] : set) = false
    | find n (x :: l) = (x = n) orelse find n l

  (* in this version, we allow you to keep duplicates when inserting *)
  fun insert (n : int) (s : set) = n :: s
  
  (* you must delete all duplicates in this *)
  (* return the set as is if n is not in the set *)
  fun delete (n : int) (s : set) = 
      List.filter (fn x => x <> n) s

  (* union is also allowed to have duplicates in it *)
  fun union (s1 : set) (s2 : set) = s1 @ s2

  (* all elements in s1 and s2 *)
  fun intersection (s1 : set) (s2 : set) = 
      List.filter (fn x => find x s2) s1

  (* s1 - s2 *)
  (* Note - we're removing all instances of the duplicate *)
  fun difference (s1 : set) (s2 : set) = 
      List.filter (fn x => not (find x s2)) s1

end 

structure TestSKD = 
struct
  val true = SetKeepDuplicates.find 1 [1,2,3]
  val [1,1,2] = SetKeepDuplicates.insert 1 [1,2]
  val [2] = SetKeepDuplicates.delete 1 [1,1,2]
  val [1,2,3,4] = SetKeepDuplicates.union [1,2] [3,4]
  val [1,2] = SetKeepDuplicates.intersection [1,2,3,4] [5,6,1,2]
  val [1,2] = SetKeepDuplicates.difference [1,2,3,4] [3,4] 
end 
