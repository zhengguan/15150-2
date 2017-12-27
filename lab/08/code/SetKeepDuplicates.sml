(* Duplicates are permitted in the insert/union functions *)
(* intset.sig contains descriptions of all the functions *)
structure SetKeepDuplicates : INTSET =
struct
  exception NYI

  type set = int list
  val empty = (fn () => [])

  fun find (n : int) (s : set) =
      case s of
        []  => false
       |x::xs => (n = x) orelse (find n xs)

  fun insert (n : int) (s : set) =
      n::s

  fun delete (n : int) (s : set) =
      case s of
        [] => []
       |x::xs => (case (x = n) of
                    true => (delete n xs)
                   |false => x::(delete n xs))

  fun singleDelete (n : int) (s : set) =
      case s of
        [] => []
       |x::xs => (case (x = n) of
                    true => xs
                   |false => x::(singleDelete n xs))

  fun union (s1 : set) (s2 : set) =
      s1@s2

  fun intersection (s1 : set) (s2 : set) =
      case (s1,s2) of
        ([],_) => []
       |(_,[]) => []
       |(x::xs,y::ys) => (case (x = y) of
                            true => x :: (intersection xs ys)
                           |false => (case ((find x ys),(find y xs)) of
                                        (true, true) =>
                   x::y::(intersection (singleDelete y xs) (singleDelete x ys))
                                       |(true, false) =>
                                      x::(intersection xs (singleDelete x ys))
                                       |(false, true) =>
                                      y::(intersection (singleDelete y xs) ys)
                                       |(false, false) => intersection xs ys))

  fun difference (s1 : set) (s2 : set) =
      case (s1,s2) of
        ([],_) => s2
       |(_,[]) => s1
       |(x::xs, y::ys) => (case ((find x s2), (find y s1)) of
                             (true, true) => (difference xs ys)
                            |(true, false) => y::(difference xs (singleDelete x ys))
                            |(false, true) => x::(difference (singleDelete y xs) ys)
                            |(false, false) => x::y::(difference xs ys))

end


structure TestSetKeepDuplicates =
struct
open SetKeepDuplicates
val true  = find 2 (2::(empty ()))
val false = find 2 (3::(empty ()))
val [2,3,2] = insert 2 [3,2]
val [3] = delete 2 [2,3,2]
end
