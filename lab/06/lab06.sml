(* listofNs : int -> int -> int list
 * REQUIRES: d >= 0
 * ENSURES: listofNs n d => a list with lengt hd where every element is n
 *)
fun listofNs (n : int) (d : int) : int list =
    case d of
      0 => []
     |_ => n :: (listofNs n (d-1))

val [] = listofNs 0 0
val [5, 5, 5] = listofNs 5 3
val [~56, ~56, ~56, ~56, ~56] = listofNs ~56 5


(* bingen : int -> int -> int list list
 * REQUIRES: m, n >= 0
 * ENSURES: bingen m n => a list of all possible binary numbers with
 * m 1s and n 0s, where each binary number is represented as
 * an int list of 1s and 0s
 *)
fun bingen (m : int) (n : int) : int list list =
    case (m, n) of
      (0, _) => [listofNs 0 n]
     |(_, 0) => [listofNs 1 m]
     |(_, _) => let
                   val L1 = map (fn l : int list => 0::l) (bingen m (n-1))
                   val L2 = map (fn l : int list => 1::l) (bingen (m-1) n)
                in
                  L1@L2
                end

fun bingenC (m : int) (n : int) (k : int list list -> 'a) : 'a =
    case (m,n) of
      (0, _) => k [listofNs 0 n]
     |(_, 0) => k [listofNs 1 m]
     |(_, _) => bingenC m (n-1) (fn x =>
                bingenC (m-1) n (fn y =>
                k ((map (fn l : int list => 1::l) y) @
                   (map (fn l : int list => 0::l) x))))

fun intListListTest (L : int list list) : int list list =
    case L of
      [] => []
     |l::ls => (map (fn x => case x of 0 => ~5 | _ => 5) l)::(intListListTest ls)
