(* ---------------------------------------------------------------------- *)
(* SECTION 2 *)

(* unzip : (string * int) list -> string list * int list
 * REQUIRES: true
 * ENSURES: unzip [(s1, i1), ..., (sn, in)] ==> ([s1, ... sn], [i1,...,in])
 *)
fun unzip (L : (string * int) list) : string list * int list =
    case L of
	[] => ([], [])
      | (x1,x2)::xs =>
	let
            val (l1,l2) = unzip xs
	in
            (x1::l1, x2::l2)
	end

(* zip : string list * int list -> (string * int) list
 * REQUIRES: true
 * ENSURES: unzip ([s1, ... sn], [i1,...,im])
       ==> [(s1, i1), ..., (sk, ik)], k = min(n,m)
 *)
fun zip (L1 : string list, L2 : int list) : (string * int) list =
    case (L1, L2) of
	([], _) => []
      | (_, []) => []
      | (x::xs, y::ys) => (x,y) :: zip (xs,ys)

(* ---------------------------------------------------------------------- *)
(* SECTION 3 *)

(* Task 3.1: Implement and document this function. *)
(* heads : int * int list -> int
 * REQUIRES: true
 * ENSURES: heads (n, l) => i, where i is the number of instances of n
 * at the front of l *)
fun heads (n : int, l : int list) : int =
    case l of
      [] => 0
     |x::xs => (case n = x of
                  true => 1 + heads(n, xs)
                 |false => 0)

val 0 = heads(53,[])
val 0 = heads(53,[1,53,2])
val 2 = heads(0,[0,0,5,0])

(* Task 3.2: Implement and document this function. *)
(* tails : int * int list -> int list
 * REQUIRES: true
 * ENSURES: tails(n, l) => l', where l' is identical to l, but with all
 * initial instances of n deleted. *)
fun tails (n : int, l : int list) : int list =
    case l of
      [] => []
     |x::xs => (case n = x of
                  false => l
                 |true  => tails(n, xs))

val [] = tails (9,[])
val [] = tails (9, [9,9,9])
val [63,7] = tails(100, [100,63,7])
val [63,7] = tails(100, [63,7])

(* Task 3.3: Implement and document this function. *)
(* filterInt : int * int list -> int list
 * REQUIRES: true
 * ENSURES: filterInt(n, l) => l', where l' is identical to l, but with
 * all instances of n removed *)
fun filterInt (n : int, l : int list) : int list =
    case l of
      [] => []
     |x::xs => (case n = x of
                  false => x::filterInt(n, xs)
                 |true  => filterInt(n, xs))

val [] = filterInt(7,[])
val [] = filterInt(7,[7,7,7])
val [4,2] = filterInt(16,[4,16,2])
val [9,81,9] = filterInt(100,[9,81,9,100])

(* ---------------------------------------------------------------------- *)
(* SECTION 4 *)

(* Task 4.1: Implement and document this function. *)
(* l_a_sHelp : int list -> int list
 * REQUIRES: l has at least two elements
 * ENSURES: l_a_sHelp c::x::xs => the look_and_say of x::xs, provided
 * c is the count of x*)
fun l_a_sHelp (l : int list) : int list =
    let
       val c::(x::xs) = l
    in
      (case xs of
           [] => c::(x::xs)
          |y::ys => (case x = y of
                       false => c::(x::l_a_sHelp(1::(y::ys)))
                      |true  => l_a_sHelp((c+1)::(x::ys))))
    end

val [3,2] = l_a_sHelp [1,2,2,2]

(* look_and_say : int list -> int list
 * REQUIRES: true
 * ENSURES: look_and_say l => l', the look_and_say version of l *)
fun look_and_say (l : int list) : int list =
    case l of
      [] => []
     |x::xs => l_a_sHelp(1::(x::xs))

val [3,2] = look_and_say [2,2,2]
val [] = look_and_say []
val [1,1,2,2] = look_and_say [1,2,2]
val [1,1] = look_and_say [1]
val [5,2,1,13] = look_and_say [2,2,2,2,2,13]

(* Task 4.2: Implement and document this function. *)
(* l_s_tHelp : int list * int -> int list
 * REQUIRES: n >= 0
 * ENSURES: l_s_tHelp (l, n) => l', the resultant list when
 * look_and_say is applied n times to l *)
fun l_s_tHelp (l : int list, n : int) : int list =
    case (l, n) of
         (_, 0) => l
        |(_, _) => l_s_tHelp((look_and_say l),(n-1))

val [1] = l_s_tHelp ([1],0)
val [1,1] = l_s_tHelp ([1],1)
val [2,1] = l_s_tHelp ([1],2)
val [1,2,1,1] = l_s_tHelp ([1],3)
val [1,1,1,2,2,1] = l_s_tHelp([1],4)
val [3,1,2,2,1,1] = l_s_tHelp([1],5)

(* look_say_table : int list * int -> int list list
 * REQUIRES: n >= 0
 * ENSURES: look_say_table (l, n) => L, a list of length n+1 of repeated
 * look_and_say functions performed on l, starting with l as the 0th list *)
fun look_say_table (l : int list, n : int) : int list list =
    case n of
         0 => [l]
        |_ => look_say_table(l, (n-1))@[l_s_tHelp(l,n)]

val [
[1],
[1, 1],
[2, 1],
[1, 2, 1, 1]
] = look_say_table([1], 3)
val [[2,3,4]] = look_say_table([2,3,4], 0)

(* ---------------------------------------------------------------------- *)
(* SECTION 5 *)

(* add_to_each : int list * int -> int list
 * REQUIRES: true
 * ENSURES: add n to each element of the list l
 *
 * Examples:
 *
 * add_to_each (nil, 7) == nil
 * add_to_each (1::2::3::nil, 3) == 4::5::6::nil
 * add_to_each (6::5::4::nil, ~3) == 3::2::1::nil
 *)
fun add_to_each (l : int list, n : int) : int list =
    case l of
        nil => nil
      | x::xs => x + n :: add_to_each (xs, n)

val [] = add_to_each ([], 7)
val [4, 5, 6] = add_to_each([1,2,3], 3)


(* Task 5.1: Implement and document this function. *)
(* prefixSum : int list -> int list
 * REQURIES: true
 * ENSURES: prefixSum L => L' where the ith index  element of L' is equal
 * to the sum of the first (i+1) elements of L *)
fun prefixSum (l : int list) : int list =
    case l of
      [] => []
     |x::xs => x::(add_to_each(prefixSum(xs), x))

val [] = prefixSum []
val [0, 1] = prefixSum [0, 1]
val [0, 1, 7, 23, 27] = prefixSum [0, 1, 6, 16, 4]
val [1, 3, 6] = prefixSum [1, 2, 3]
val [5, 8 ,9] = prefixSum [5, 3, 1]
val [5, 8 ,4] = prefixSum [5, 3, ~4]

(* Task 5.3: Implement and document this function. *)
(* prefixSumHelp : int * int list -> int list
 * REQUIRES: true
 * ENSURES: prefixSumHelp (x, L) => L', where the ith index element of L' is
 * equal to the ith index element of L plus the sum of the previous
 * i elements of L starting with element x *)
fun prefixSumHelp (x : int, L : int list) : int list =
    case L of
      [] => []
     |y::ys => (x+y)::prefixSumHelp((x+y),ys)

val [1,4,7] = prefixSumHelp (1, [0,3,3])
val [6,10,12] = prefixSumHelp (3, [3, 4, 2])
val [~2, 5, 7] = prefixSumHelp (~2, [0,7,2])
val [] = prefixSumHelp (42,[])

(* prefixSumFast : int list -> int list
 * REQUIRES: true
 * ENSURES: prefixSumFast L => L' where the ith index element of L' is equal
 * to the sum of the first (i+1) elements of L, but FAST *)
fun prefixSumFast (l : int list) : int list =
    case l of
      [] => []
     |x::xs => x::prefixSumHelp(x, xs)

val [] = prefixSumFast []
val [0, 1] = prefixSumFast [0, 1]
val [0, 1, 7, 23, 27] = prefixSumFast [0, 1, 6, 16, 4]
val [1, 3, 6] = prefixSumFast [1, 2, 3]
val [5, 8 ,9] = prefixSumFast [5, 3, 1]
val [5, 8 ,4] = prefixSumFast [5, 3, ~4]

(* ---------------------------------------------------------------------- *)
(* SECTION 6 *)

(* Task 6.1: Implement and document this function. *)
(* contains : int * int list -> bool
 * REQUIRES: true
 * ENSURES: contains (x, l) => true if x is an element of l, false otherwise
 *)
fun contains (x : int, l : int list) : bool =
   case l of
     [] => false
    |y::ys => (x = y) orelse (contains(x, ys))

val false = contains (17, [])
val true = contains (63, [7, 4, 17, 63])
val true = contains (~8, [5, ~8])

(* remove : int * int list -> int list
 * REQUIRES: x is an element of l, len(l) >= 1
 * ENSURES: remove(x, l) => l', a list identical to l but with the first
 * instance of x removed *)
fun remove (x : int, l : int list) : int list =
  let
     val y::ys = l
  in
    (case x = y of
       true => ys
      |false => y::remove(x, ys))
  end

val [2,4] = remove (3, [2, 3, 4])
val [6, 7, 8] = remove (9, [6, 7, 8, 9])
(* Why was 6 afraid of 7? 'Cause 7 8 9!*)
val [6, 7, 9] = remove (9, [6, 7, 9, 9])

(* subset : int list * int list -> int list
 * REQUIRES: true
 * ENSURES: subset (s, l) => true if every element of s appears in l,
 * counting duplicates, and false otherwise *)
fun subset (s : int list, l : int list) : bool =
   case s of
     [] => true
    |x::xs => (case contains(x, l) of
                false => false
               |true  => subset(xs, remove(x, l)))

val true = subset ([],[])
val true = subset ([12, 4, 3], [12, 5, 6, 3, 4])
val false = subset ([12, 4, 4, 3], [12, 5, 6, 3, 4])
val false = subset ([0, 1], [2])
val false = subset ([0, 1], [1])
val true = subset ([1, 2, 2], [5, 2, 7, 1, 3, 8, 2])

(* Task 6.2: Implement and document this function. *)
(* subsequence : int list * int list -> bool
 * REQUIRES: true
 * ENSURES: subsequence (p, l) => true if p is a subsequence of l, and
 * false otherwise *)
fun subsequence (p : int list, l : int list) : bool =
  case (p, l) of
   ([],_) => true
  |(x::xs, []) => false
  |(x::xs, y::ys) => (case x = y of
                        true => subsequence (xs, ys)
                       |false => subsequence (x::xs, ys))

val false = subsequence ([1, 2, 3], [])
val true = subsequence ([2, 4], [1, 2, 3, 4])
val true = subsequence ([1, 2], [1, 3, 2])
val false = subsequence ([3, 1], [1, 3, 2])
val true = subsequence ([], [~5, 3, 4])

(* Task 6.3: Implement and document this function. *)
(* subrun : int list * int list -> bool
 * REQUIRES: true
 * ENSURES: subrun (s, l) => true if s is a subrun of l, and false otherwise
 *)
fun subrun (s : int list, l : int list) : bool =
   case (s, l) of
    ([],_) => true
   |(_ ,[]) => false
   |([x],_) => contains (x, l)
   |( _ ,[p]) => false
   |(x::(y::xs), p::(q::ps)) => (case (x = p, y = q) of
                                  (true, false) => false
                                 |(true, true) => subrun(y::xs,q::ps)
                                 |(false, _ ) => subrun (x::(y::xs),q::ps))

val true = subrun ([2, 3], [1, 2, 3, 4])
val false = subrun ([2, 4], [1, 2, 3, 4])
val true = subrun ([~1, 5, 7], [42, ~1, 5, 7, 81, 100])
val false = subrun([~1, 5, 7], [42, ~1, 5, 81, 7, 100])
val true = subrun ([], [1, 2, 3, 4])

(* ---------------------------------------------------------------------- *)
(* SECTION 7 *)

(* Task 7.1: Implement and document this function. *)
(* subset_sum : int list * int -> bool
 * REQUIRES: true
 * ENSURES: subset_sum (l, s) => true if there exists a subset of l that
 * sums to s, and false otherwise *)
fun subset_sum (l : int list, s : int) : bool =
    case l of
      [] => (s = 0)
     |x::xs => (subset_sum(xs, s-x)) orelse (subset_sum(xs, s))

val true = subset_sum([], 0)
val false = subset_sum([], ~1)
val true = subset_sum([1, 2, 1, ~6, 10], 4)
val true = subset_sum([42,~26, 13, ~16], 0)

(* Task 7.2: Implement and document this function. *)
(* subset_sum_cert : (int list * int) -> bool * int list
 * REQURIES: true
 * ENSURES: subset_sum_cert (l, s) => (true, U) if U is a subset of l that
 * sums to s, and (false, []) otherwise *)
fun subset_sum_cert (l : int list, s : int) : bool * int list =
    case l of
      [] => (s = 0, [])
     |x::xs => (let
                  val (P, U) = subset_sum_cert(xs, s - x)
                in
                  (case P of
                     true => (true, x::U)
                    |false => subset_sum_cert(xs, s))
                end)

val (true, []) = subset_sum_cert([], 0)
val (false, []) = subset_sum_cert([], ~1)
val (true, [1, 2, 1]) = subset_sum_cert([1, 2, 1, ~6, 10], 4)
val (true, [42,~26, ~16]) = subset_sum_cert([42,~26, 13, ~16], 0)
