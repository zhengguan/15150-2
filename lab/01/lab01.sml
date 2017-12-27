ource and freely distributable
~
~                           Sponsor Vim development!
* ---------------------------------------------------------------------- *)
(* For Sec 7.1 *)
fun intToString (x : int) : string = Int.toString x
(* ---------------------------------------------------------------------- *)

(* ---------------------------------------------------------------------- *)

(* Code from class *)

(* sum : int list -> int *)
(* REQUIRES: true *)
(* ENSURES: sum(L) evaluates to the sum of the integers in L. *)
fun sum (L : int list): int =
  case L of
    [] => 0
  | x::xs => x + (sum xs)

(* count : int list list -> int *)
(* REQUIRES: true *)
(* ENSURES: count(R) evaluates to the sum of the integers in R. *)
fun count (L : int list list): int =
  case L of
    [] => 0
  | r::rs => (sum r) + (count rs)
