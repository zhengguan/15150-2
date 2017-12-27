(***** Section 3 Options ******)
(* Task 3.1 *)

(* findEven: int list -> int option *)
(* REQUIRES : true *)
(* ENSURES : findEven(L) returns SOME(x) iff
 * there is at least one even number x in L *)
fun findEven ([] : int list): int option = NONE
  | findEven (x :: xs) = case x mod 2 of
                           0 => SOME x
                         | _ => findEven xs

val NONE = findEven [1,3,5]
val NONE = findEven []
(* Note for these we're just testing that we get back SOME.
 * so saying val SOME x would be sufficient as well *)
val SOME 2 = findEven [2,4,6]
val SOME 4 = findEven [3,4,5,6,7,8,9]

(* subset_sum_cert provided for your convenience *)
fun subset_sum_cert (nil : int list, s : int): bool * int list = (s = 0, nil)
  | subset_sum_cert (x :: xs, s) =
    case subset_sum_cert (xs, s - x) of
      (true, l1) => (true, x::l1)
    | (false, _) => subset_sum_cert (xs, s)

(* Task 3.2 *)

(* subset_sum_option : int list * int -> int list option *)
(* REQUIRES: true *)
(* ENSURES: subset_sum_option(l,s) == SOME l' iff l' is a sublist
 * of l that sums to s and NONE if no such l' exists *)
fun subset_sum_option (l : int list, 0 : int) : int list option = SOME []
  | subset_sum_option ([], _) = NONE
  | subset_sum_option (x :: xs, s) =
    case subset_sum_option (xs, s - x) of
      SOME (l) => SOME (x :: l)
    | NONE => subset_sum_option (xs, s)

val NONE = subset_sum_option ([],1)
val NONE = subset_sum_option ([1,2,3], ~1)
val SOME [] = subset_sum_option ([], 0)
val SOME [] = subset_sum_option ([1,2,3,4], 0)

(* Section 4: Propositions *)
(* Task 4.1 *)

datatype prop =
    True
  | False
  | And of (prop * prop)
  | Or of (prop * prop)
  | Imp of (prop * prop)
  | Not of (prop)

(* Task 4.2 *)
(* eval_prop: prop -> bool *)
fun eval_prop (True : prop) : bool = true
  | eval_prop (False) = false
  | eval_prop (And(p1, p2)) = eval_prop(p1) andalso eval_prop(p2)
  | eval_prop (Or(p1, p2)) = eval_prop(p1) orelse eval_prop(p2)
  | eval_prop (Imp(p1, p2)) =
      (case (eval_prop(p1)) of
         true => eval_prop(p2)
       | false => true)
  | eval_prop (Not(p1)) = not (eval_prop(p1))

val p1 = And(True, False)
val p2 = Imp(True, False)
val p3 = False
val p4 = Not(p3)
val p5 = Or(p1, p4)

val true = eval_prop p5
val false = eval_prop p3
val false = eval_prop p2
val true = eval_prop (Or(p3, p4))


(* Section 5: Binops *)

datatype binop =
    Add
  | Subtract
  | Multiply

fun eval_binop (b : binop, x : int, y : int) : int =
    case b of
        Add => x + y
      | Subtract => x - y
      | Multiply => x * y

(* Task 5.1 *)

fun foldl_binop (b : binop, L : int list) : int =
    case L of
    [] => if b = Multiply then 1 else 0
  | [x] => x
  | x::y::xs => foldl_binop (b, (eval_binop (b, x, y))::xs)

(* type binop = int * int -> int *)

(* Task 5.2 *)

datatype exptree =
    Const of int
  | Binop of binop * exptree * exptree

(* Task 5.3 *)

fun evaluate (et : exptree) : int =
    case et of
        Const x => x
      | Binop (f, et1, et2) => eval_binop (f, evaluate et1, evaluate et2)

(* val Add = op+ *)
(* val Subtract = op- *)
(* val Multiply = op* *)
(* val Max = Int.max *)
(* val Mod = Int.mod *)
(* val Divide = Int.div *)

val et1 = Binop (Add, Binop (Multiply, Const 3, Const 5), Const 8)
val et2 = Binop (Subtract, Binop (Multiply, Const 9, Const 5), Const 20)
val et3 = Binop (Add, et1, et2)

val 23 = evaluate et1
val 25 = evaluate et2
val 48 = evaluate et3

datatype sint = Zero
              | Pos of int
              | Neg of int

(* Section 6: Signs Point to No *)
(* Task 6.1 *)

fun sint_plus (X : sint, Y : sint) : sint =
    case (X,Y) of
    (Zero,_) => Y
  | (_,Zero) => X
  | (Pos x, Pos y) => Pos (x+y)
  | (Neg x, Neg y) => Neg (x+y)
  | (Pos x, Neg y) => (case (Int.sign (x-y)) of
                        ~1 => Neg (y-x)
                      | 0 => Zero
                      | 1 => Pos (x-y)
                      | _ => Zero) (* shouldn't happen *)
  | (Neg x, Pos y) => (case (Int.sign (y-x)) of
                        ~1 => Neg (x-y)
                      | 0 => Zero
                      | 1 => Pos (y-x)
                      | _ => Zero) (* shouldn't happen *)

fun sint_minus (X : sint, Y : sint) : sint =
    case (X,Y) of
    (Zero,_) => (case Y of
                        Zero => Zero
                      | Pos v => Neg v
                      | Neg v => Pos v)
  | (_,Zero) => X
  | (Pos x, Neg y) => Pos (x+y)
  | (Neg x, Pos y) => Neg (x+y)
  | (Pos x, Pos y) => (case (Int.sign (x-y)) of
                        ~1 => Neg (~x+y)
                      | 0 => Zero
                      | 1 => Pos (x-y)
                      | _ => Zero) (* shouldn't happen *)
  | (Neg x, Neg y) => (case (Int.sign (y-x)) of
                        ~1 => Neg (x-y)
                      | 0 => Zero
                      | 1 => Pos (y-x)
                      | _ => Zero) (* shouldn't happen *)

(* Task 6.2 *)

fun sint_compare (X : sint, Y : sint) : order =
    case (sint_minus (X,Y)) of
    Neg x => LESS
  | Zero => EQUAL
  | Pos x => GREATER


(* Section 8: Jpop World *)

type title = string
datatype artist = Akb | MomoiroCloverZ | MorningMusume
type duration = int

datatype song = Song of title * artist * duration
type musicCollection = song list

(* Task 8.1 *)

(* stringSubstring : string * string -> bool *)
(* REQUIRES: true *)
(* ENSURES: stringContains (s, t) returns whether s is a substring of t *)
fun isSubstring (s : string, t : string) = String.isSubstring s t

val isMomoclo : song -> bool =
    fn Song (t, a, d) => (a = MomoiroCloverZ)

val containsKoi : song -> bool =
    fn Song (t, a, d) => isSubstring ("koi", t)

val isShort : song -> bool =
    fn Song (t, a, d) => (d < 4)

(* Task 8.2 *)
(* filter : ((song -> bool) * musicCollection) -> musicCollection *)
(* REQUIRES: f is total *)
(* ENSURES: filter (f,L) == L' where x \in L' only if f(x) = true. L' will
 * also respect ordering *)
fun filter (f : song -> bool, [] : musicCollection) : musicCollection = []
  | filter (f, x :: xs) = if f x then x :: filter(f, xs) else filter(f, xs)

val senpaiMusic : musicCollection =
  [
    Song ("koi suru fortune cookie", Akb, 5),
    Song ("hashire", MomoiroCloverZ, 4),
    Song ("flying get", Akb, 4),
    Song ("love machine", MorningMusume, 5),
    Song ("mite mite kochichi", MomoiroCloverZ, 3)
  ]

val [Song ("hashire", MomoiroCloverZ, 4),
     Song ("mite mite kochichi", MomoiroCloverZ, 3)] =
     filter (isMomoclo, senpaiMusic);

val [Song ("koi suru fortune cookie", Akb, 5)] =
     filter (containsKoi, senpaiMusic);

val [Song ("mite mite kochichi", MomoiroCloverZ, 3)] =
     filter (isShort, senpaiMusic);



(* Task 8.3 *)

(* filter' : ('a -> bool) * 'a list -> 'a list *)
(* REQUIRES: f is total *)
(* ENSURES: filter' (f,L) == L' where x \in L' only if f(x) = true. L' will
 * also respect ordering *)
fun filter' (f : 'a -> bool, [] : 'a list) : 'a list = []
  | filter' (f, x :: xs) = if f x then x :: filter'(f, xs) else filter'(f, xs)

val even : int -> bool = fn x => x mod 2 = 0
val neg : int-> bool = fn x => x < 0

val [2,4,6] = filter' (even, [1,2,3,4,5,6])
val [] = filter' (even, [])
val [] = filter' (even, [1,3,5])
val [~3,~2,~1] = filter' (neg, [~3,~2,~1,0,1,2,3])
val [] = filter' (neg, [])
val [] = filter' (neg, [1,2,3])


(* YOU SHALL NOT PASS *)



(* CHALLENGE ACCEPTED *)


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


(* addition : church numeral * church numeral -> church numeral *)
fun addition  (n, m) = fn (base, iter) => (n (m, addOne)) (base, iter)
fun addition' (n, m) = fn (base, iter) => n ((m (base, iter)), iter)

(* UNCOMMENT WHEN YOU ARE READY TO TEST *)

val 0 = convert (addition (zero, zero))
val 2 = convert (addition (one, one))
val 5 = convert (addition (two, three))


(* multiplication : church numeral * church numeral -> church numeral *)
fun multiplication (n, m) = fn (base, iter) => m (base, fn x => n (x, iter))

(* UNCOMMENT WHEN YOU ARE READY TO TEST *)

val 0 = convert (multiplication (zero, zero))
val 1 = convert (multiplication (one, one))
val 6 = convert (multiplication (two, three))


(* pair *)
fun pair (x, y) = (fn f => f (x, y))

(* first *)
fun fst (x, y) = x

(* second *)
fun snd (x, y) = y

(* subOne : church numeral -> church numeral *)
fun subOne n =
  (n (pair (zero, zero), fn p => pair (addOne (p fst), p fst))) snd

(* UNCOMMENT WHEN YOU ARE READY TO TEST *)

val 0 = convert (subOne one)
val 2 = convert (subOne three)
val 0 = convert (subOne zero)

exception Take15312
