(* Imperative programming with lists *)
structure MutableList =
struct
  datatype 'a cell = Nil
                   | Cons of 'a * 'a llist
  withtype 'a llist = ('a cell) ref

  (*Task 2.1*)
  (* tolist : 'a llist -> 'a list
   * REQUIRES: true
   * ENSURES: tolist l1 => l1 as a list instead of a llist
   *)
  fun tolist (l1 : 'a llist) : 'a list =
     case !l1 of
       Nil => []
      |Cons(x,xs) => x::(tolist xs)

val [] = tolist (ref(Nil))
val empty : int llist = ref(Nil)
val l1 : int llist = ref(Cons(1,empty))
val l2 : int llist = ref(Cons(2,l1))
val [2,1] : int list = tolist l2

  (*Task 2.2*)
  (* map : ('a -> 'a) -> 'a llist -> unit
   * REQUIRES: f is total
   * ENSURES: map f l updates every element x in l with (f x)
   *)
  fun map (f : 'a -> 'a) (l : 'a llist) : unit =
      case !l of
        Nil => ()
       |Cons(x,xs) => ((map f xs);
                       (l := (Cons((f x),xs))))

  (*Task 2.4*)
  (* filter : ('a -> bool) -> 'a llist -> unit
   * REQUIRES: p is total
   * ENSURES: filter p l updates l such that only elements that satisfy p
   *          remain in l
   *)
  fun filter (p : 'a -> bool) (l : 'a llist) : unit =
     case !l of
       Nil => ()
      |Cons(x,xs) => ((filter p xs);(case (p x) of
                                    true => ()
                                   |false => (l := !xs)))

  (*Task 2.6*)
  (* append : ('a llist * 'a llist) -> unit
   * REQUIRES: true
   * ENSURES: append (l1,l2) modifies l1, replacing the end of it with l2,
   *          and keeps l2 unchanged
   *)
  fun append (l1 : 'a llist, l2 : 'a llist) : unit =
     case !l1 of
       Nil => (l1 := !l2)
      |Cons(x,xs) => append (xs,l2)

end

structure TestMutableList =
struct
  structure ML = MutableList
  open ML
  val test1 : int llist = ref (Cons (1, ref (Cons (2, ref (Cons (3, ref Nil))))))
  val test2end : int llist = ref Nil
  val test2 : int llist = ref (Cons (4, ref (Cons (5, ref (Cons (6, test2end))))))
  val () = append(test1,test2)
  val [1,2,3,4,5,6] = tolist test1
  val [4,5,6] = tolist test2
  val () = (test2end := Cons(7, ref Nil))
  val [4,5,6,7] = tolist test2
  val [1,2,3,4,5,6,7] = tolist test1
end


(* For all existing t, eq(t,new()) == false *)
signature NEW = sig
    type t
    val new : unit -> t
    val eq  : t * t -> bool
end

structure IntNew = struct
datatype t = I of int
fun new () = raise Fail "unimplemented"
fun eq _ =raise Fail "unimplemented"
end

structure RefNew = struct
type t = unit ref
fun new () = raise Fail "unimplemented"
fun eq _ = raise Fail "unimplemented"
end

structure ExnNew = struct
type t = exn * (exn -> bool)
fun new () =
    let
	exception Eq
    in
	(Eq, fn Eq => true | _ => false)
    end

fun eq ((e1,f1),(e2,f2)) = f1 e2
end

signature UNION_FIND =
sig
    type table
    val init  : int -> table
    val unify : table -> int -> int -> unit
    val eq    : table -> int -> int -> bool
end

functor UnionFind (N:NEW):UNION_FIND=
struct

  datatype cell =
             Var of N.t
           | Ptr of ptr
  withtype ptr = cell ref

  type table = ptr Seq.seq

  fun init n = raise Fail "unimplemented"

  fun compress r =raise Fail "unimplemented"

  fun unify s i j =raise Fail "unimplemented"

  fun eq s i j =raise Fail "unimplemented"
end


functor TestIt(UF: UNION_FIND) = struct
	val uf = UF.init 10
	val () = UF.unify uf 0 1
	val () = UF.unify uf 2 3
	val () = UF.unify uf 0 3
	val true = UF.eq uf 0 1
	val true = UF.eq uf 1 2
	val true = UF.eq uf 2 3
	val false = UF.eq uf 3 6
	val true = UF.eq uf 6 6
	(* Add your own tests here *)
end

(* Test the implementations here*)
