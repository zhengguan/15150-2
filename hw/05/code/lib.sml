CM.make "rationals/sources.cm";
type rat = Rational.t

(********** library functions for rationals **********)

(* Defining infix operators for rats *)
infixr 9 //
infixr 7 ++
infixr 7 --
infixr 8 **

(* // : int * int -> rat
 * REQUIRES: d <> 0
 * ENSURES: n // d evaluates to a rat that represents the rational number given
 * by dividing n / d
 *)
fun (n : int) // (d : int) = Rational.fromPair (n, d)

(* ++ : rat * rat -> rat
 * REQUIRES: true
 * ENSURES: x ++ y evaluates to a rat that represents the rational number given
 * by adding x + y
 *)
fun (x : rat) ++ (y : rat) : rat = Rational.plus(x, y)

(* -- : rat * rat -> rat
 * REQUIRES: true
 * ENSURES: x -- y evaluates to a rat that represents the rational number given
 * by adding x - y
 *)
fun (x : rat) -- (y : rat) : rat = Rational.subtract(x, y)

(* ** : rat * rat -> rat
 * REQUIRES: true
 * ENSURES: x ** y evaluates to a rat that represents the rational number given
 * by adding x * y
 *)
fun (x : rat) ** (y : rat) : rat = Rational.times(x, y)

(* ~~ : rat -> rat
 * REQUIRES: true
 * ENSURES: ~~ x evaluates to a rat that represents the rational number given
 * by negating x
 *)
val ~~ : rat -> rat = Rational.negate

(* divide : rat * rat -> rat
 * REQUIRES: y <> 0
 * ENSURES: divide(x,y) evaluates to a rat that represents the rational number
 given by dividing x / y
 *)
val divide : rat * rat -> rat = Rational.divide

(* ratEq : rat * rat -> bool *)
(* REQUIRES: true
 * ENSURES: ratEq (r1, r2) returns true iff r1 = r2 *)
fun ratEq (r1, r2) = case Rational.compare (r1, r2) of
      EQUAL => true
    | _ => false

(* r2s : rat -> string
 * REQUIRES: true
 * ENSURES: r2s r evaluates to a string that represents the rat r *)
val r2s = Rational.toString

(********** library functions for polynomials **********)

(* represent c_0 x^0 + c_1 x + c_2 x^2 + ...
   by the function that maps
   the natural number i to the coefficient c_i
*)
type poly = int -> rat

(* nth : 'a list * int -> 'a option
 * REQUIRES: true
 * ENSURES: nth(l,n) evalutes to SOME(k) for element k at the nth index of l.
 * nth(l,n) evaluates to NONE if there is not an nth element in l
 *)
fun nth (l : 'a list, n : int) : 'a option =
    case l of
        nil => NONE
      | x::xs => (case n of
                      0 => SOME x
                    | _ => nth (xs, n-1))

(* listToFun : 'a * 'a list -> (int -> 'a)
 * REQIURES: true
 * ENSURES: listToFun(x, l) evaluates to a function that maps the int i
 * to the element at index i in l if i < the length of l. If i >= length of
 * l, then the function maps i to x.
 *)
fun listToFun (x : 'a, l : 'a list) : int -> 'a =
    fn y => case nth (l, y) of NONE => x | SOME x' => x'

(* polyToString : poly * int -> string
 * REQUIRES: count >= 0, n is a valid poly
 * ENSURES: polyToString(n, count) evaluates to a string that
 * represents count number of terms from the polynomial represented by n
 *)
fun polyToString (n : poly, count : int) : string =
    let
      fun termToString (c : rat, e : int) : string =
        r2s c ^ "x^" ^ Int.toString e
    in
      (String.concatWith " + "
                         (List.tabulate(count+1, fn e => termToString(n e, e)))
      ) ^ " + ...\n"
    end

(* polynomialEqual : poly * poly * int -> bool
 * REQUIRES: count >= 0, n1 and n2 are valid poly
 * ENSURES: polynomialEqual(n1, n2, count) evaluates to true if n1 and n2
 * represent the same polynomial for count terms and false otherwise
 *)
fun polynomialEqual (n1 : poly, n2 : poly, count : int) : bool =
    ListPair.all (fn (r1, r2) => EQUAL = Rational.compare (r1,r2))
                 (List.tabulate(count+1, n1), List.tabulate(count+1, n2))

(********** library functions for Anshu's Potluck **********)
(* split: 'a list -> ('a list * 'a list) *)
(* REQUIRES: true *)
(* ENSURES: split L => (A, B) s.t. L = A@B,
								|length A - length B| <= 1 *)
fun split ([] : 'a list) : ('a list * 'a list) = ([], [])
	| split ([x]) = ([x], [])
	| split (x :: y :: L) =
		let
			val (L', R') = split L
		in
			(x :: L', y :: R')
		end
(* merge: ('a * 'a -> order) -> ('a list * 'a list) -> 'a list *)
(* REQUIRES: f is total *)
(* ENSURES: merge f (A, B) => L s.t. L = A @ B,
									 L is sorted by the function f *)
fun merge (f : 'a * 'a -> order) ([] : 'a list, B : 'a list) = B
	| merge f (A, []) = A
	| merge f (x :: A, y :: B) = case f (x, y) of
		LESS => x :: (merge f (A, y :: B))
		| EQUAL => x :: y :: (merge f (A, B))
		| GREATER => y :: (merge f (x :: A, B))

(* msort: ('a * 'a -> order) -> 'a list -> 'a list *)
(* REQUIRES: f is a total function *)
(* ENSURES: msort f L => L', L' is a sorted permutation of L in O(n log n) *)
fun msort (f : 'a * 'a -> order) ([] : 'a list) : 'a list = []
	| msort f [x] = [x]
	| msort f L =
		let
			val (A, B) = split L
		in
			merge f (msort f A, msort f B)
		end

exception IntervalException
