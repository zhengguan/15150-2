(* you can remove this definition when you're done to make sure you didn't
 * miss any functions
 *)
exception Unimplemented

(* evenP : int -> bool
 * REQUIRES : n >= 0
 * ENSURES  : evenP(n) evaluates to true if n is even and false otherwise *)
fun evenP(n : int) : bool =
    case n
     of 0 => true
      | 1 => false
      | _ => evenP(n-2)

(* Task 2.1 *)
(* evens : int list -> int list *)
fun evens (L : int list) : int list =
   case L of
     [] => []
    |x::xs => (case evenP x of
                 false => evens xs
                |true  => x::(evens xs))

val [0,0,4] = evens [0,0,4]
val [] = evens []
val [0,0,4,2] = evens [0,0,4,9,3,2]

(* Task 2.2 *)
(*coolHelp1 : (int * string) list * int -> int
 *REQUIRES:
 *ENSURES: coolHelp1(L, m) => the maximum n of each element (n,s) in L
 * given current maximum m *)
fun coolHelp1 (L : (int * string) list, m : int) : int =
    case L of
      [] => m
     |x::xs => (let
                   val (i, s) = x
                in
                  (case i > m of
                     true => coolHelp1(xs, i)
                    |false => coolHelp1(xs, m))
                end)

val 5 = coolHelp1 ([1(,"s"), (5, "t"), (2, "r")],~1)
val ~1 = coolHelp1 ([],~1)

(* coolHelp2 : (int * string) list -> (int * string) list *)
(* ENSURES: coolHelp2 L => L sorted by the ints *)
fun coolHelp2 (L : (int * string) list) : (int * string) list =
 case L of
   [] => []
  |[x] => [x]
  |x::(y::xs) => (case x < y of
                    true => x::(coolHelp2(y::xs))
                   |false => y::(coolHelp2(x::xs)))

(* coolHelp3 : (int * string) list * int * int
 * ENSURES: coolHelp3 (L', max, currI) => string option list of L *)
fun coolHelp3 (L' : (int * string) list, max : int, currI : int) : (string option) list =
    case L' of
        [] => []
       |x:xs => (let
                    val (i,s) = x
                 in
                   (case currI = i of
                         true => SOME(s)::coolHelp3(xs,max,currI+1)
                        |false =>

(* coolSort : (int * string) list -> (string option) list *)
fun coolSort (L:(int * string) list): (string option) list =
    case L of
      [] => []
     |x::xs => (let
                   val m = coolHelp1(L, 0)
                   val L' = coolHelp2(L)
                in
                  coolHelp3(L, [], m, 0)
                end)

val [NONE, SOME("s"), SOME("r"), NONE, NONE, SOME("t")] =
   coolSort [(1,"s"), (5,"t"), (2,"r")]

(* fib : int -> int
 * REQUIRES : n >= ~1
 * ENSURES  : fib n evaluates to the nth Fibonaci number *)
fun fib (n : int) : int =
    case n
     of ~1 => 0
      | 0 => 1
      | 1 => 1
      | _ => fib(n-1) + fib(n-2)

(* Task 4.2 *)
(* fastfib : int -> int * int *)
fun fastfib _ = raise Unimplemented
