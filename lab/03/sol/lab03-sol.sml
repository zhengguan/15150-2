(* evenP : int -> bool
 * REQUIRES : n >= 0
 * ENSURES  : evenP(n) evaluates to true if n is even and false otherwise *)
fun evenP(n : int) : bool =
    case n
     of 0 => true
      | 1 => false
      | _ => evenP(n-2)

(* Task 2.1 *)
(* evens : int list -> int list
 * evens take an int list and removes all odd elements
 *   leaving the remaining events in the same order
 *
 * REQUIRES: true
 * ENSURES: returns a list containing only even ints, in
 *   the order they were in the original list
 *
 * examples:
 *   evens([1,2,3,4,5]) ==> [2,4]
 *   evens([]) ==> []
 *   evens([0,2,4,6]) ==> [0,2,4,6]
 *   evens([1,3,5]) ==> []
 *)

fun evens ([] : int list) : int list = []
  | evens (x::xs : int list) : int list =
       if evenP x then x :: evens (xs)
       else evens (xs)

val [2,4] = evens [1,2,3,4,5]
val [] = evens []
val [0,2,4,6] = evens [0,2,4,6]
val [] = evens [1,3,5]

(* Task 2.2 *)
(* credit: int list -> int list
 * REQUIRES: true
 * ENSURES: returns a list containing the updated amounts
 * payable to each ta, after $5 was credited to each of them
 *
 * examples:
 *   credit([11, 10, 13, 11, 15]) ==> [16, 15, 18, 16, 20]
 *   credit([2, 1, 4, 5]) ==> [7, 6, 9, 10]
 *   credit([]) ==> []
 *   credit([0]) ==> [5]
 *)

fun credit ([] : int list) : int list = []
  | credit (x::xs : int list) : int list = (x + 5) :: credit(xs)

(* Tests for credit *)
val [16, 15, 18, 16, 20] = credit([11, 10, 13, 11, 15])
val [5] = credit([0])


(* Task 2.3 *)
(* reverse: int list -> int list *)
(* REQUIRES: true *)
(* ENSURES: reverse(L) evalautes to a list that contains all
 *  the elements of L in reverse order.
 *)
fun reverse (L : int list) : int list =
    let
      fun reverse' (L : int list, A : int list) : int list =
        case L of
            [] => A
           | x::R => reverse' (R, x :: A)
    in
      reverse' (L, [])
    end

(* tests for reverse *)
val [] = reverse []
val [2,1] = reverse [1,2]
val [1,2,2,3,4,5,5] = reverse [5,5,4,3,2,2,1]


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
(* fastfib : int -> int * int
 * REQUIRES: n >= 0
 * ENSURES: fastfib n == (fib (n - 1) , fib n)
 *
 * examples:
 *  fastfib(12) ==> (144, 233)
 *  fasefib(0) ==> (0,1)
 *  fastfib(1) ==> (1,1)
 *)

fun fastfib (0 : int) : int * int = (0, 1)
  | fastfib (n : int) : int * int =
      let
        val (x : int , y : int) = fastfib (n - 1)
      in
        (y , x + y)
      end

(* tests for fastfib *)
val (0,1) = fastfib 0
val (1,1) = fastfib 1
val (144, 233) = fastfib 12

