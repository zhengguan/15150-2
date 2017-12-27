val _ = use "lib.sml";     (* DO NOT CHANGE THIS LINE *)

datatype 'a shrub =
   Leaf of 'a
 | Branch of 'a shrub * 'a shrub

(* Section 2: Exceptions *)
exception NotFound

(* Section 3: Total Functions *)

(* findOne: ('a -> bool) -> 'a shrub -> 'a
 * REQUIRES: p is total
 * ENSURES:  evaluates to s x, where x is the leftmost element of t that satisfies p
	     or k() if no such element exists.
 *)
fun findOne (p : 'a -> bool) (t : 'a shrub) (s: 'a -> 'b) (k:unit -> 'b): 'b =
  case t of
    Leaf(x) => if p(x) then s x else k()
  | Branch(L,R) => findOne p L s (fn () => findOne p R s k)

(* search: ('a -> bool) -> 'a shrub -> 'a
 * REQUIRES: p is total
 * ENSURES:  evaluates to the leftmost element of t that satisfies p
	     or raises an exeption if no such element exists.
 *)
fun search (p : 'a -> bool) (t : 'a shrub) : 'a =
  case t of
    Leaf(x) => if p(x) then x else raise NotFound
  | Branch(L,R) => search p L handle NotFound => search p R


(* task 3.2 *)
(* search : ('a -> bool) -> 'a shrub -> 'a option
 * REQUIRES: p is total
 * ENSURES: search' p t ==> SOME x, where x is an element in t
 * such that p(x) => true, and ==> NONE if such an element
 * does not exist
 *)
fun search' (p : 'a -> bool) (t : 'a shrub) : 'a option =
    SOME(search p t) handle NotFound => NONE

val NONE = search' (fn s => String.size(s) > 1)(Leaf(""))
val SOME("") = search' (fn s => String.size(s) > ~1)(Leaf(""))
val SOME(5) = search' (fn x => (x=5)) (Branch(Branch(Leaf 7, Leaf 8),Branch(Leaf 5, Leaf 7)))
val NONE = search' (fn x => (x=5)) (Branch(Branch(Leaf 7, Leaf 8),Branch(Leaf 6, Leaf 7)))

(* Section 4: It's My Own Invention *)
datatype square = Free | Wall | Target

exception NoPath
exception CorporealnessSucks
exception Success of ((int * int) list)

(* Task 3.1 *)
(* mazeSolver : square list list -> (int * int) -> 'a
 * REQUIRES: board is a rectangular 2D list, and
 *           start represents a (row, column) pair that is within
 *           the bounds of board
 * ENSURES: mazeSolver board start ==> raise Success L, where L is
 * an (int * int) list representing a valid path from start to the Target
 * square if one exists, and ===> raise NoPath if no such path exists.
 * Also ==> raise CorporealnessSucks if start is a Wall square
 *)
fun mazeSolver (board : square list list) (start : (int * int)) =
   let
      val (row, col) = start
   in
     case List.nth(List.nth(board,row),col) of
       Target => raise Success [start]
      |Wall => raise CorporealnessSucks
      |Free => (mazeSolver board (row+1,col))
               handle Success L => raise (Success (start::L))
                     |_ =>
               ((mazeSolver board (row, col+1))
               handle Success L => raise (Success (start::L))
                     |_ => raise NoPath)
   end

val NONE = SOME(mazeSolver [[Free, Free, Wall],
                            [Free, Wall, Free],
                            [Free, Wall, Target]] (0,0)) handle (Success L) => SOME (L) | _ => NONE
val SOME [(0,0),(1,0),(2,0),(2,1),(2,2)] = SOME(mazeSolver [[Free, Free, Wall],
                            [Free, Wall, Free],
                            [Free, Free, Target]] (0,0)) handle (Success L) => SOME (L)
val SOME("win!") = SOME(mazeSolver [[Free, Free, Wall],
                            [Free, Wall, Free],
                            [Free, Free, Target]] (0,0)) handle (Success L) => SOME ("win!") | CorporealnessSucks => SOME("this does suck") | _ => NONE
val SOME("this does suck") = SOME(mazeSolver [[Free, Free, Wall],
                            [Free, Wall, Free],
                            [Free, Free, Target]] (0,2)) handle (Success L) => SOME ("win!") | CorporealnessSucks => SOME("this does suck") | _ => NONE
val SOME("aww") = SOME(mazeSolver [[Free, Free, Free],
                            [Free, Wall, Wall],
                            [Free, Free, Target]] (0,2)) handle (Success L) => SOME ("win!") | CorporealnessSucks => SOME("this does suck") | NoPath => SOME("aww")
(* Section 2: Regexp *)


(* match : regexp -> char list -> (char list -> bool) -> bool
 * REQUIRES:  r is in standard form, k is total.
 * ENSURES: match r cs k == true iff there is a split p@s==cs such that
 *            p is in language of r and k s == true.
 *)
fun match (r : regexp) (cs : char list) (k : char list -> bool) : bool =
    case r of
        Zero => false
      | One => k cs
      | Char c => (case cs of
                       []  => false
                     | c' :: cs' => chareq(c,c') andalso k cs')
      | Plus (r1,r2) => match r1 cs k orelse match r2 cs k
      | Times (r1,r2) => match r1 cs (fn cs' => match r2 cs' k)
      | Star r =>
            let fun matchrstar cs = k cs orelse match r cs matchrstar
            in
                matchrstar cs
            end
      | All =>
        let
          fun matchfront cs' =
              case cs' of
                nil => k nil
              | _::cs'' => k cs' orelse matchfront cs''
        in
          matchfront cs
        end
      (* Task 2.2 *)
      | Both (r1,r2) => match r1 cs (fn cs' =>
                        match r2 cs (fn cs'' =>
                        charlisteq(cs',cs'') andalso k cs'))

val reg1 = Times(Char #"a", Char #"b")
val reg2 = Star(Char #"a")
val reg3 = Times(Star(Char #"a"),Char #"b")
val false = match (Both(reg1, reg2)) [#"a",#"b"] (fn l => true)
val true  = match (Both(reg1, reg3)) [#"a",#"b"] (fn l => true)


(* Task 2.1: badBoth *)
(* badBoth: regexp -> regexp -> char list -> (char list -> bool) -> bool
* REQUIRES: (standard requirements as for match)
* ENSURES: badBoth r1 r2 cs k evalutes to true iff there exist values p,s
* such that p@s is equivalent to cs, p is in L(Both(r1,r2)), and k s is
* true.
*)
fun badBoth (r1: regexp) (r2:regexp) (cs: char list) (k: char list ->
  bool) : bool =
  (match r1 cs k) andalso (match r2 cs k)

val true = badBoth reg1 reg2 [#"a",#"b"] (fn l => true)
(* This shouldn't happen according to the spec!*)

(* Task 2.4: badDiff *)
(* badDiff: regexp -> regexp -> char list -> (char list -> bool) -> bool
* REQUIRES: (standard requirements as for match)
* ENSURES: badDiff r1 r2 cs k evalutes to true iff there exist values p,s
* such that p@s is equivalent to cs, p is in L(Diff(r1,r2)), and k s is
* true.
*)
fun badDiff (r1: regexp) (r2:regexp) (cs: char list) (k: char list ->
  bool) : bool =
  (match r1 cs k) andalso not (match r2 cs k)

val false = badDiff (Char(#"a")) (Times(Char #"a",Char #"b")) [#"a", #"b"] (fn l => true)
(* this should evaluate to true according to the spec!*)

(* accept : regexp -> string -> bool
 * REQUIRES: r is in standard form.
 * ENSURES: accept r s   evaluates to true if and only if s is in the
 *          language of r *)
fun accept r s = match r (String.explode s) (fn [] => true | _ => false)


(* Tests for Both (r1, r2) *)
val true = accept (Both(Times(Char #"a",Char #"b"),Times(Char #"a",Plus(Char #"a", Char #"b")))) "ab"
val false = accept (Both(One, Char #"a")) "ab"
val true = accept (Both(One, Star(Char #"a"))) ""

(* Tasks 2.5 *)
val messageKey : regexp = Times(
                          Times(
                          Times(Plus(Plus(Char #"f", Char #"u"), Char #"n"),
                                Plus(Plus(Char #"a", Char #"r"), Char #"e")),
                                Plus(Plus(Char #"v", Char #"a" ),Char #"l")),
                                All)

val false = accept messageKey "uem1234f"
val true = accept messageKey "uel0EoN03U"


(* Task 2.6 *)
fun findMessage (r: regexp) (message : string list) : string =
  case message of
    [] => ""
   |s::ss => (case (accept r s) of
                true => String.str(String.sub(s, 4)) ^ (findMessage r ss)
               |false => findMessage r ss)
