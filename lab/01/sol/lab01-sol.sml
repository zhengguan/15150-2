(* ---------------------------------------------------------------------- *)
(* For Sec 7.1 *)
fun intToString (x : int) : string = Int.toString x
(* ---------------------------------------------------------------------- *)

(* ---------------------------------------------------------------------- *)

(* Code from class *)

(* sum : int list -> int *)
(* REQUIRES: true *)
(* ENSURES: sum(L) evaluates to the sum of the integers in L. *)
fun sum (L : int list) : int = 
  case L of
    [] => 0
  | x::xs => x + (sum xs)

(* count : int list list -> int *)
(* REQUIRES: true *)
(* ENSURES: count(R) evaluates to the sum of the integers in R. *)
fun count (L : int list list) : int = 
  case L of
    [] => 0
  | r::rs => (sum r) + (count rs)

(* Task 7.2 *)
(* mult: int list -> int *)
(* REQUIRES: true *)
(* ENSURES: mult(L) evaluates to the product of integers in L *)
fun mult (L : int list): int = 
  case L of
    [] => 1
  | x::xs => x * mult(xs)

val 1 = mult []
val 5 = mult [5]
val 42 = mult [2, 21]

(* Task 7.3 *)
(* mult' : int list * int -> int *)
(* REQUIRES: true *)
(* ENSURES: mult'(L, a) evaluates to the product of integers in L times a *)
fun mult' (L : int list, a : int) : int = 
  case L of
    [] => a
  | x::xs => mult'(xs, x * a)

val 10 = mult' ([], 10)
val 3 = mult' ([3], 1)
val 60 = mult' ([2, 1, 15], 2)

(* Task 7.4 *)
(* Mult : int list list -> int *)
(* REQUIRES: true *)
(* ENSURES: Mult(R) evaluates to the product of integers in R *)

fun Mult (L : int list list) : int = 
  case L of 
    [] => 1
  | r::rs => (mult r) * (Mult rs)

val 1 = Mult []
val 1 = Mult [[]]
val 10 = Mult [[10]]
val 35 = Mult [[7, 5]]
val 21 = Mult [[1, 3], [7]]

(* Task 7.5 *)
(* Mult : int list list * int -> int *)
(* REQUIRES: true *)
(* ENSURES: Mult (R, a) evaluates to the product of integers in R times a *)

fun Mult' (L : int list list, a : int) =
  case L of
    [] =>  a
  | r::rs => Mult' (rs, mult' (r, 1) * a)

val 5 = Mult' ([], 5)
val ~16 = Mult' ([[~4]], 4)
val 180 = Mult' ([[2, 2, ~1], [15, ~1], [3]], 1)
