exception Unimplemented

(* ===== Code provided for you ===== *)

datatype binop =
    Add
  | Subtract
  | Multiply

fun eval_binop (b : binop, x : int, y : int) : int =
    case b of
        Add => x + y
      | Subtract => x - y
      | Multiply => x * y

datatype sint = Zero
              | Pos of int
              | Neg of int


(* ===== Your code begins here ===== *)

(* Task 3.1 *)
(* findEven : int list -> int option *)
fun findEven (L : int list) : int option =
    case L of
      [] => NONE
     |x::xs => (case (x mod 2) = 0 of
                  true => SOME x
                 |false => findEven xs)

val SOME(2) = findEven [1,2,3,5,7]
val SOME(2) = findEven [1,3,5,7,2]
val NONE = findEven [1,3,5,7,9]

(* Task 3.2 *)
(* subset_sum_option : int list * int -> int list option *)
fun subset_sum_option (L : int list, s : int) : int list option =
    case L of
      [] => (case s = 0 of
               true => SOME(L)
              |false => NONE)
     |x::xs => (case subset_sum_option (xs, s-x) of
                  NONE => subset_sum_option (xs, s)
                 |SOME(L') => SOME(x::L'))


(* Section 5: Jpop World *)

type title = string
datatype artist = Akb | MomoiroCloverZ | MorningMusume
type duration = int

datatype song = Song of title * artist * duration
type musicCollection = song list

(* Task 5.1 *)

val isMomoclo : song -> bool =
    fn Song (_, artist, _) => (artist = MomoiroCloverZ)

(* stringSubstring : string * string -> bool *)
(* REQUIRES: true *)
(* ENSURES: stringContains (s, t) returns whether s is a substring of t *)
fun stringSubstring (s : string, t : string) = String.isSubstring s t

val containsKoi : song -> bool =
    fn Song (_, _, _) => raise Unimplemented

val isShort : song -> bool =
    fn Song (_, _, _) => raise Unimplemented

(* Task 5.2 *)
fun filter (_ : (song -> bool), _ : musicCollection) : musicCollection =
    raise Unimplemented

val senpaiMusic : musicCollection =
  [
  Song ("koi suru fortune cookie", Akb, 5),
  Song ("hashire", MomoiroCloverZ, 4),
  Song ("flying get", Akb, 4),
  Song ("love machine", MorningMusume, 5),
  Song ("mite mite kochichi", MomoiroCloverZ, 3)
  ]

(* Task 5.3 *)
fun filter' (_ : ('a -> bool), _ : 'a list) : 'a list =
    raise Unimplemented


(* Task 6.1 *)
(* define your datatype prop here *)
datatype prop = ImplementMe

(* Task 6.2 *)
(* eval_prop : prop -> bool *)
fun eval_prop (P : prop) : bool = raise Unimplemented

(* Task 7.1 *)
(* foldl_binop : binop * int list -> int *)
fun foldl_binop (b : binop, L : int list) : int = raise Unimplemented

(* Task 7.2 *)
datatype binop_tree = ImplementMe

(* Task 7.3 *)
(* eval_tree : binop_tree -> int *)
fun eval_tree (T : binop_tree) : int = raise Unimplemented

(* Task 8.1 *)
(* sint_plus : sint * sint -> sint *)
fun sint_plus (X : sint, Y : sint) : sint = raise Unimplemented

(* sint_minus : sint * sint -> sint *)
fun sint_minus (X : sint, Y : sint) : sint = raise Unimplemented

(* Task 8.2 *)
(* sint_compare : sint * sint -> order *)
fun sint_compare (X : sint, Y : sint) : sint = raise Unimplemented

(* YOU SHALL NOT PASS *)



(* CHALLENGE ACCEPTED *)


(* ===== Code provided for you ===== *)

val zero = fn (base, iter) => base
val one = fn (base, iter) => iter base
val two = fn (base, iter) => iter (iter base)
val three = fn (base, iter) => iter (iter (iter base))


(* addOne : church numeral -> int *)
(* REQUIRES: true *)
(* ENSURES: converts a church numeral to its respective natural number *)
fun convert n : int = n (0, fn x => x + 1)

(* addOne : church numeral -> church numeral *)
(* REQUIRES: true *)
(* ENSURES: if n = (convert i), then addOne n = (convert (i + 1)) *)
fun addOne n = fn (base, iter) => iter (n (base, iter))

val 1 = convert (addOne zero)
val 3 = convert (addOne (addOne one))


(* ===== Your code begins here ===== *)

(* addition : church numeral * church numeral -> church numeral *)
fun addition (n, m) = raise Unimplemented

(* UNCOMMENT WHEN YOU ARE READY TO TEST *)
(*
val 0 = convert (addition (zero, zero))
val 2 = convert (addition (one, one))
val 5 = convert (addition (two, three))
*)


(* multiplication : church numeral * church numeral -> church numeral *)
fun multiplication (n, m) = raise Unimplemented

(* UNCOMMENT WHEN YOU ARE READY TO TEST *)
(*
val 0 = convert (multiplication (zero, zero))
val 1 = convert (multiplication (one, one))
val 6 = convert (multiplication (two, three))
*)


(* subOne : church numeral -> church numeral *)
fun subOne n = raise Unimplemented

(* UNCOMMENT WHEN YOU ARE READY TO TEST *)
(*
val 2 = convert (subOne)
val 2 = convert (subOne three)
val 0 = convert (subOne zero)
*)
