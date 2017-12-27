(* READ THIS COMMENT!
 *
 * In this file there are various lines marked by a comment either like so:
 *
 *    raise Fail "unimplemented"  (* DELETE THIS LINE *)
 *
 * or like so:
 *
 *    _ = raise Fail "unimplemented"  (* DELETE THIS LINE *)
 *
 * You do not need to delete these lines immediately, but they should be gone by
 * the time you hand in your homework. They are placeholders for your
 * implementations of the functions specified in the homework.  Without them,
 * this file would not load.
 *
 * If you remove such a line without implementing the function it is associated
 * with, this file will not load. Only remove such lines when you are ready to
 * implement their associated function.
 *
 *   Also notice that for some functions we specified the left parts of
 *   (one or more) function clauses, but for other functions we did not.
 *   You will need to decide what the function clauses should be in those cases.
 *
 * Remember to test your code! Place tests immediately following the function
 * you have written.
 *)

(* REQUIRES:  n, m >= 0
 * ENSURES:  add (n, m) ==> n + m
 *)
fun add (n : int, m : int) : int =
  case n of
    0 => m
  | _ => 1 + add (n-1, m)

(* Task 5.1: Implement and document this function. *)
(* Note: You may want to pattern-match for different cases. *)
(* mult : int * int -> int *)
(* REQUIRES: n, m >= 0
 * ENSURES: mult (n,m) ==> n * m
 *)
fun mult (x : int, y : int) : int =
    case x of
         0 => 0
        |_ => add (y, mult((x-1),y))

val 0 = mult(0,1)
val 0 = mult(1,0)
val 1 = mult(1,1)
val 73 =mult(1,73)
val 12 = mult(3,4)
val 12 = mult(4,3)
val 25 = mult(5,5)

(* Task 5.2: Implement and document this function. *)
(* pascal : int * int -> int
 * REQUIRES: i,j >= 0, i >= j
 * ENSURES: pascal (i,j) ==> element of Pascal's triangle at row i and column j
 *)
fun pascal (i : int, j : int) : int =
    case (i,j) of
         (_,0) => 1
        |(_,_) => case (i = j) of
                        true => 1
                       |false => pascal(i-1,j-1) + pascal(i-1,j)

(*TESTING GOES HERE*)
val 1 = pascal(0,0)
val 1 = pascal(5,0)
val 1 = pascal(13,13)
val 6 = pascal(4,2)
val 4 = pascal(4,1)
val 3 = pascal(3,2)

(* Task 5.3: Implement and document this function. *)
(* divmod : int * int -> int * int
 * REQUIRES: n >= 0, d > 0
 * ENSURES: divmod(n,d) ==> (q,v) for quotient q and remainder r from division of n by d
 *)
fun divmod ((n, d) : int * int) : int * int =
    case (n < d) of
         true => (0, n)
        |false => (let
                      val (q, r) = divmod((n-d), d)
                   in
                     (q+1, r)
                   end)

val (1,0) = divmod(1,1)
val (1,0) = divmod(100,100)
val (1,2) = divmod(5,3)
val (0,0) = divmod(0,5)
val (0,2) = divmod(2,7)
val (5,0) = divmod(25,5)
val (3,0) = divmod(18,6)
val (3,1) = divmod(19,6)

(* Task 5.4: Implement this function. *)

(* evenP : int -> bool
 *REQUIRES: n >= 0
 *ENSURES: evenP n ==> true if n is even, false otherwise
 *)
fun evenP (n : int) : bool =
    case n of
         0 => true
        |1 => false
        |_ => evenP (n-2)

val true = evenP 0
val false = evenP 1
val true = evenP 8
val false = evenP 19

(* primeHelper : int * int -> bool
 * REQUIRES: n >= 3, n >= i, i >= 2
 * ENSURES: primeHelper(n,i) ==> true if (n mod i = 0) for any i <= n, i >= 2
 *)
fun primeHelper(n : int, i : int) : bool =
    case (i = n) of
          true => true
         |false => (case ((n mod i) = 0) of
                    false => primeHelper(n, i+1)
                   |true  => false)

val true = primeHelper(3,2)
val true = primeHelper(5, 2)
val true = primeHelper(3,2)
val false = primeHelper(6,2)
val true = primeHelper(7, 2)

(* is_prime : int -> bool
 * REQUIRES: n >= 0
 * ENSURES: is_prime n ==> true if n is prime, false otherwise
 *)
fun is_prime (n : int) : bool =
    case n of
      0 => false
     |1 => false
     |2 => true
     |_ => primeHelper(n,2)

val false = is_prime 0
val false = is_prime 1
val true  = is_prime 2
val true  = is_prime 3
val true  = is_prime 13
val false = is_prime 14
val false = is_prime 35
