use "lib.sml";

val zero : rat = 0//1
val one : rat = 1//1
val half : rat = 1//2

(* ---------------------------------------------------------------------- *)
(* Section 4 - Polynomials as SML functions *)
(* Task 4.1 *)
(* differentiate : poly -> poly
 * REQUIRES: true
 * ENSURES: differentiate p ==> p', where p' : int -> rat such that
 * p(i) ==> c, the coefficient of the ith term of the derivative of p
 *)
fun differentiate (p : poly) : poly =
    (fn i => ((i+1)//1) ** p(i+1))

val poly1 : poly  = (fn i : int => (case i of
                      0 => 1//1
                     |1 => 1//2
                     |2 => 7//1
                     |_ => 0//1))
val poly2 : poly = (fn i : int => (case i of
                      0 => 1//2
                     |1 => 14//1
                     |_ => 0//1))
val poly3 : poly = (fn i : int => (case i of
                      0 => 14//1
                     |1 => 1//1
                     |2 => 1//4
                     |3 => 7//3
                     |_ => 0//1))

val true = polynomialEqual(poly2, differentiate(poly1), 0)
val true = polynomialEqual(poly2, differentiate(poly1), 1)
val true = polynomialEqual(poly2, differentiate(poly1), 13)
val true = polynomialEqual(poly1, differentiate(poly3), 1)

(* Task 4.2 *)
(* integrate : poly -> (rat -> poly)
 * REQUIRES: true
 * ENSURES: integrate p ==> P : rat -> poly, a function that accepts an
 * argument r : rat and returns a polynomial equal to the integral of
 * p with a constant r
 *)
fun integrate (p : poly) : rat -> poly =
    fn r =>
       (fn i =>
           (case i of
              0 => r
             |_ => divide( (p (i-1)), (i//1) )
             ))

val true = polynomialEqual(poly1:poly,((integrate poly2) (1//1)):poly, 0)
val true = polynomialEqual(poly1,(integrate poly2) (1//1), 1)
val true = polynomialEqual(poly3, ((integrate poly1) (14//1)):poly, 0)
val true = polynomialEqual(poly1, (differentiate( integrate poly1 (0//1))), 2)

(* ---------------------------------------------------------------------- *)
(* Section 5 - Write and Prove *)
(* Task 5.1 *)
(* concat : 'a list list -> 'a list
 * REQUIRES: true
 * ENSURES: concat L ==> L' : 'a list, a flattened version of L with exactly
 * the elements of each sub-list in L
 *)
fun concat (L : 'a list list) : 'a list =
    case L of
      [] => []
     |[]::ls => concat ls
     |(x::xs)::ls => x::concat(xs::ls)

val [] = concat []
val [] = concat [[]]
val [[]] = concat [[[]]]
val [1] = concat [[1]]
val ["a", "b", "z"] = concat [[], ["a","b"], [], [], [], ["z"]]
val [1,2,5,6,10,10] = concat[[1,2],[5,6],[],[10,10]]

(* ---------------------------------------------------------------------- *)
(* Section 6 - Anshu's Potluck Party  *)
(* Put your datatype declaration here, if you're going to be using one. *)
(* Task 6.1 *)
(* getLast : 'a list -> 'a
 * REQUIRES: L is a non-empty, sorted list
 * ENSURES: getLast L ==> x, the last (greatest) element in L
 *)
fun getLast (L : 'a list) : 'a =
    case L of
      [] => raise Fail "lel not happening"
     |[x] => x
     |x::xs => getLast xs

val 5 = getLast [0,1,2,4,5]
val 0 = getLast [0]

(* getFreeTimes : int list * int list -> int -> (int * int) list
 * REQUIRES: starts and ends are sorted lists of the same length
 * ENSURES: getFreeTimes (starts, ends) minStart ==> a list of
 * (s,e) intervals representing start and end times of
 * free intervals in between the busy intervals outlined by
 * starts and ends, starting with minStart
 *)
fun getFreeTimes (starts : int list, ends : int list) (minStart : int) :
    (int * int) list =
    case (starts, ends) of
      ([],[]) => []
      |(_,[]) => []
      |([],x::xs) => [(minStart,getLast(ends))]
      |(x::xs, y::ys) => (case minStart < x of
                            true => (minStart,x)::
                                    (getFreeTimes (xs,ys) y)
                           |false => getFreeTimes (xs,ys) y)

val [] = getFreeTimes ([],[]) 0
val [] = getFreeTimes ([0,1,7],[1,7,10]) 0
val [(1,2),(5,8)] = getFreeTimes([0,2,8],[1,5,170]) 0

(* mergeFreeIntervs : ((int * int) list * (int * int) list) -> (int * int) list
 * REQUIRES: free1 and free2 are sorted lists of integer tuples (s,e),
 * where s < e, representing intervals of free time
 * ENSURES: mergeFreeIntervs (free1, free2) ==> a list of integer tuples
 * representing the overlapping intervals of free1 and free2
 *)
fun mergeFreeIntervs (free1 : (int * int) list, free2 : (int * int) list) :
                     (int * int) list =
    case (free1, free2) of
     (_, []) => []
     |([],_) => []
     |((s1,e1)::xs,(s2,e2)::ys) =>
      case Int.compare(s1,s2) of
        LESS => (case e1 > s2 of
                   true => (s2, e1)::(mergeFreeIntervs(xs, free2))
                  |false => mergeFreeIntervs(xs, free2))
       |EQUAL => (case Int.compare(e1,e2) of
                    LESS => (s1,e1)::(mergeFreeIntervs(xs,free2))
                   |EQUAL => (s1,e1)::(mergeFreeIntervs(xs,ys))
                   |GREATER => (s2,e2)::(mergeFreeIntervs(free1,ys)))
       |GREATER => (case e2 > s1 of
                      true => (s1,e2)::(mergeFreeIntervs(free1,ys))
                     |false => mergeFreeIntervs(free1,ys))

val [] = mergeFreeIntervs ([],[(0,1),(2,3)])
val [] = mergeFreeIntervs([(0,1),(2,3)],[])
val [(2,3),(5,7)] = mergeFreeIntervs ([(0,3),(4,7),(9,12)],
                                     [(2,4),(5,8)])

(* all_available: (int * int) list list -> (int * int) list
 * REQUIRES: if L is non-empty, for each sub-list in L consisting of intervals
 * (s : int, e : int), s < e
 * ENSURES: all_available L ==> l, where l is a list of start and end intervals
 * that fit in between the "busy" intervals in each schedule list in L
*)
fun all_available (L : (int * int) list list) : (int * int) list =
    case L of
      [] => []
     |_ => let
             fun startList (intervs : (int * int) list): int list =
                 msort Int.compare (map (fn (s,e) => s) intervs)
             fun endList (intervs : (int * int) list) : int list =
                 msort Int.compare (map (fn (s,e) => e) intervs)
             val startsEnds = map (fn intervs : (int * int) list =>
                                      (startList intervs, endList intervs)) L
             val freeTimes : (int * int) list list = map
                 (fn (sList : int list, eList : int list) =>
                     getFreeTimes (sList, eList) 0) startsEnds
             val (initial::rest) = freeTimes                                               in
               foldr mergeFreeIntervs initial rest
            end

val [] = all_available []
val [(1,2)] = all_available [[(0,1), (2,3)]]
val [(0,1),(3,4)] = all_available [[(1,2),(4,6),(8,12)],
                                   [(1,3),(5,9)]]

(* Task 6.2 *)
(* oddP : int -> bool
 * REQUIRES: true
 * ENSURES: oddP x ==> true if x is odd, and ==> false if x is even
 *)
fun oddP (x : int) =
    (x mod 2) = 1

val true = oddP 1
val false = oddP 0
val false = oddP 32
val true = oddP ~1

(* filter : ('a -> bool) -> ('a list) -> ('a list)
 * REQUIRES: true
 * ENSURES: filter test L ==> L', a list with all the
 * elements in L that satisfy test
 *)
fun filter (test : 'a -> bool) (L : 'a list) : 'a list =
    case L of
      [] => []
     |x::xs => (case test x of
                  true => x::(filter test xs)
                 |false => filter test xs)

val [1,11,13] = filter oddP [1,2,4,6,8,10,11,12,13]

(* all_good_available:
 * REQUIRES: if L is non-empty, for each sub-list in L consisting of intervals
 * (s : int, e : int), s < e. Also, n >=0
 * ENSURES: all_good_available (L,n) ==> a list of intervals of free times
 * (s,e) such that (s-e) >= n
 *)
fun all_good_available (L: ((int * int) list list), n : int) : (int * int) list =
  filter (fn (s,e) => ((e-s) = n orelse (e-s) > n)) (all_available L)

val [] = all_good_available ([],0)
val [(1,2)] = all_good_available ([[(0,1), (2,3)]],1)
val [] = all_good_available ([[(0,1), (2,3)]],2)
val [(3,5)] = all_good_available ([[(1,2),(5,6),(8,12)],
                                   [(1,3),(5,9)]], 2)

(* ---------------------------------------------------------------------- *)
(* Section 7 - Polymorphism, HOFs, Options *)
(* Task 7.1 *)
(* transpose : 'a list list -> 'a list list
 * REQUIRES: true
 * ENSURES: transpose L ==> L', a list containing 'a lists with the number
 * of rows and columns switched, and containing the same elements as L
 *)
fun transpose (L : 'a list list) : 'a list list =
    case L of
      [] => []
     |l::ls => (let
                  fun getElem (l : 'a list) : int -> int -> 'a option =
                      (fn targI : int  =>
                          (fn currI : int =>
                              case l of
                                [] => NONE
                               |x::xs => (case currI = targI of
                                        true => SOME(x)
                                       |false => getElem xs targI (currI+1))))

                  fun collectElem (i : int) : 'a list list -> 'a list =
                      (fn L =>
                         (case L of
                           [] => [] (* no more lists!*)
                          |l::ls => (case (getElem l i 0) of
                                       NONE => []
                                     |SOME(x) => x::(collectElem i ls))))

                  fun collectAll (maxLength : int) : ('a list list) -> int -> 'a list list =
                      (fn L : 'a list list =>
                          (fn index : int =>
                          case index of
                            0 => []
                           |_ => (collectElem (maxLength-index) L) ::
                                 (collectAll maxLength L (index-1))))
                 in
                  collectAll (List.length l) L (List.length l)
                 end)

val [] = transpose []
val [] = transpose [[]]
val [] = transpose [[],[],[]]
val [[1],[2]] = transpose [[1,2]]
val [[1,3],[2,4]] = transpose [[1,2],[3,4]]
val [[1,3,5],[2,4,6]] = transpose [[1,2],[3,4],[5,6]]
val [[1,3],[2]]  = transpose [[1,2],[3]]

(* Task 7.2 *)
(* extract : ('a -> bool) * 'a list -> ('a * 'a list) option
 * REQUIRES: true
 * ENSURES: extract (p, L) ==> SOME(x, L') if there exists an  element x in L such that p x => true,
 * where L' is identical to L, but with THE first instance of such an x removed, and ==> NONE if
 * such an x does not exist
 *)
fun extract (p : 'a -> bool, L : 'a list) : ('a * 'a list) option =
    case L of
      [] => NONE
     |x::xs => (case (p x) of
                  true => SOME(x, xs)
                 |false => (case extract (p, xs) of
                              NONE => NONE
                             |SOME(x', xs') => SOME(x',x::xs')))

val SOME(3, [2,4]) = extract(oddP, [2,3,4])
val NONE = extract(oddP, [2,4,6])
val SOME(1, [0,2,3,4,5]) = extract(oddP,[0,1,2,3,4,5])
val NONE = extract (oddP, [])
val SOME ("b", ["aaa", "bca"]) = extract (fn s => String.size s < 2, ["aaa", "b", "bca"])
val NONE = extract (fn s => String.size s = 4, ["hallo", "testerino", "hahahaha"])
val SOME("hallo", ["testerino","hahahaha"]) = extract (fn s => String.size s  > 4, ["hallo", "testerino", "hahahaha"])

(* ---------------------------------------------------------------------- *)
(* Section 8 - Blocks World *)
(* Task 8.1 *)
(* extractMany : ('a * 'a -> bool) * ('a list) * ('a list) -> ('a list) option
 * REQUIRES: eq is total
 * ENSURES: extractMany (eq, toExtract, from) ==> SOME(L) if toExtract
 * is a sub-multiset of from, where L is identical to from, but with all
 * the elements of toExtract removed, or  ==> NONE if toExtract is not
 * a sub-multiset of from
 *)
fun extractMany (eq : 'a * 'a -> bool, toExtract : 'a list, from : 'a list) : ('a list) option =
    case toExtract of
      [] => SOME(from)
     |x::xs => (case from of
                  [] => NONE
                 |y::ys => case (extract(fn elem => eq (elem,x),from)) of
                             NONE => NONE
                            |SOME(elem, from') => extractMany(eq, xs, from'))

val SOME [3,3,4,2] = extractMany(op =, [2,1,2], [1,2,3,3,2,4,2])
val SOME [poly2, poly3] = extractMany(fn (p1:poly,p2:poly) => polynomialEqual(p1,p2,0), [poly1], [poly1, poly2, poly3])
val NONE = extractMany(op =, [2,2], [2])


(* Task 8.2 *)
datatype block = X
                |Y
                |Z

datatype move = Pickup_from_table of block
               |Pickup_from_block of (block * block)
               |Put_on_block of (block * block)
               |Put_on_table of block

datatype fact = Free of block
               |On of (block * block)
               |On_table of block
               |Hand_empty
               |Hand_holding of block

type state = fact list

(* Task 8.3 *)
val initial : state = [Hand_empty,Free X, Free Y, Free Z, On_table X, On_table Y, On_table Z]

(* instantiates extractMany with equality for your fact datatype *)
fun extractManyFacts (toConsume : fact list, s : state) : state option =
    extractMany (fn (x : fact, y : fact) => x = y, toConsume, s)

(* Task 8.4 *)
(* consumeAndAdd : (state * fact list * fact list) -> state option
 * REQUIRES: bef is a list of facts to be removed from s, and
 * aft is a list of new facts to be added to s
 * ENSURES: consumeAndAdd (s, bef, aft) ==> SOME s' if bef is a sub-multiset
 * of s, where s' is s with before removed and after added, and ==> NONE if
 * before is not a sub-multiset of s
 *)
fun consumeAndAdd (s : state, bef : fact list, aft : fact list) : state option =
    case extractManyFacts(bef, s) of
      NONE => NONE
     |SOME(s') => SOME(s'@aft)

val NONE = consumeAndAdd ([],[Free X, Hand_holding Y], [On (Y,X), Hand_empty, Free Y])
val SOME([Free Y, Free Z, On_table Y, On_table Z, Hand_holding X]) = consumeAndAdd (initial,[Free X, On_table X, Hand_empty], [Hand_holding X])

(* Task 8.5 *)
(* step : (move * state) -> state option
 * REQUIRES: true
 * ENSURES: step (m, s) ==> SOME s' if the "before" facts of m hold in s, and
 * ==> NONE otherwise
 *)
fun step (m : move, s : state) : state option =
    case m of
      Pickup_from_table b => consumeAndAdd(s,
                                           [Free b, On_table b, Hand_empty],
                                           [Hand_holding b])
     |Pickup_from_block (a,b) => consumeAndAdd(s,
                                           [Free a, On (a,b), Hand_empty],
                                           [Free b, Hand_holding a])
     |Put_on_block (a, b) => consumeAndAdd(s,
                                           [Free b, Hand_holding a],
                                           [Free a, Hand_empty, On (a,b)])
     |Put_on_table b => consumeAndAdd(s,
                                    [Hand_holding b],
                                    [Hand_empty, On_table b, Free b])

val SOME([Free X, Free Z, On_table X, On_table Z, Hand_holding Y]) = step(Pickup_from_table Y, initial)
val SOME([Free Z, On_table X, On_table Z, Free Y, Hand_empty, On (Y, X)]) =
step(Put_on_block (Y, X), [Free X, Free Z, On_table X, On_table Z, Hand_holding Y])
val NONE = step(Pickup_from_table Z, [Free X, Free Y, On (X, Z), On_table Z, Hand_empty])
