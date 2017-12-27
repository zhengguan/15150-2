(* TASK 2.1 *)

(* summ : int -> int *)
(* REQUIRES: n >= 0 *)
(* ENSURES: summ n returns the sum of the integers 0 through n. *)
fun summ (0) = 0
   |summ (n : int) = n + summ(n-1)

val 0 = summ(0)
val 1 = summ(1)
val 15  = summ(5)

(* TASK 2.2 *)

(* double : int -> int *)
(* REQUIRES: n >= 0 *)
(* ENSURES: double n evaluates to 2 * n. *)
fun double (0 : int) : int = 0
  | double n = 2 + double (n - 1)

(* Tests for double *)
val 0 = double 0
val 4 = double 2

(* square : int -> int *)
(* REQUIRES: n >= 0 *)
(* ENSURES: square n evaluates to n * n.  *)
fun square (0) = 0
   |square (n) = square(n-1) + double(n) - 1

val 0 = square 0
val 1 = square 1
val 25 = square 5

(* TASK 2.3 *)

(* divisibleByThree : int -> bool *)
(* REQUIRES: n >= 0 *)
(* ENSURES: divisibleByThree n evaluates to true if n mod 3 = 0, else false *)
fun divisibleByThree (n : int) : bool =
    case n of
    0 => true
   |1 => false
   |2 => false
   |_ => divisibleByThree (n-3)

val true  = divisibleByThree 0
val false = divisibleByThree 1
val false = divisibleByThree 2
val true = divisibleByThree 21
val false = divisibleByThree 22

(* TASK 3.1 *)

(* oddP: int -> bool *)
(* REQUIRES: n >= 0 *)
(* ENSURES: oddP n evaluates to true iff n is odd, else false *)
fun oddP (n : int) : bool =
    case n of
    0 => false
   |1 => true
   |_ => oddP (n-2)

val true = oddP 1
val false = oddP 0
val true = oddP 11
val false = oddP 10

(* TASK 4.1 *)

(* GCD: int * int -> int *)
(* REQUIRES: m, n >= 0    *)
(* ENSURES: GCD (m, n) returns the g.c.d. of m and n *)
fun GCD (m : int, 0) : int = m
  | GCD (0, n : int) : int = n
  | GCD (m : int, n : int) : int =
        case m > n of
            true => GCD(m mod n, n)
          | false => GCD(m, n mod m)

(* Tests for GCD *)
val 5 = GCD (5, 0)
val 9 = GCD (0, 9)
val 6 = GCD (36, 66)

(* evenP : int -> bool *)
(* REQUIRES: n >= 0 *)
(* ENSURES: evenP n evaluates to true if n mod 2 = 0 *)
(*          evenP n evaluates to false if n mod 2 = 1 *)
fun evenP (0 : int) : bool = true
  | evenP 1 = false
  | evenP n = evenP (n - 2)

(* Tests for evenP *)
val true = evenP 0
val false = evenP 1
val true = evenP 12
val false = evenP 27

(* oddP : *)
(* REQUIRES: *)
(* ENSURES: *)

(* stein : int * int -> int *)
(* REQUIRES: m, n >= 0 *)
(* ENSURES: stein(m,n) returns the g.c.d. of m and n. *)
fun stein(m,n)=
  case (m=0) of
       true => n
     | false =>
         (case (n = 0) of
              true => m
            | false =>
                (case m = n of
                     true => m
                   | false =>
                       (case (m mod 2 = 0) of
                            true => (case (n mod 2 = 0) of
                                         true => 2 * stein(m div 2, n div 2)
                                       | false => stein(m div 2, n))
                          | false => (case (n mod 2 = 0) of
                                          true => stein(m, n div 2)
                                        | false =>
                                            (case (n>m) of
                                                 true => stein(m, (n-m) div 2)
                                               | false => stein((m-n) div 2,n))))))


(* TASK 5.1 *)

(* stein' : int * int -> int *)
(* REQUIRES: m, n > 0 *)
(* ENSURES: stein'(m,n) returns the g.c.d. of m and n. *)
fun stein'(m : int, n : int) : int =
    case (m,n,evenP m, evenP m,(m>n)) of
    (0,_,_,_,_) => n
    |(_,0,_,_,_) => m
    |(_,_,true,_,_) => stein(m div 2, n)
    |(_,_,_,true,_) => stein(m, n div 2)
    |(_,_,_,_,true) => stein(m mod n, n)
    |(_,_,_,_,false)=> stein(m, n mod n)

val 0 = stein'(0,0)
val 5 = stein'(0,5)
val 5 = stein'(5,0)
