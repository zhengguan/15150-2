(* Imperative programming with lists *)
structure MutableList =
struct
  datatype 'a cell = Nil
                   | Cons of 'a * 'a llist
  withtype 'a llist = ('a cell) ref


  (* tolist : 'a llist -> 'a list
   * REQUIRES: true
   * ENSURES: creates the corresponding 'a list from an 'a llist
   *)
  fun tolist (l1 : 'a llist) : 'a list =
      case l1 of
           ref Nil => []
         | ref (Cons(x,xs)) => x :: tolist xs

  (* map : ('a -> 'a) -> 'a llist -> unit
   * REQUIRES: f is total
   * ENSURES: mutates l so that each element x is replaced by f x
   *)
  fun map (f : 'a -> 'a) (l : 'a llist) : unit =
      case l of
           ref Nil => ()
         | ref (Cons (x,xs)) => (map f xs; l := Cons(f x,xs))

  (* filter ('a -> bool) -> 'a llist -> unit
   * REQUIRES: p is total
   * ENSURES: mutates l to include each element x iff p x
   *)
  fun filter (p : 'a -> bool) (l : 'a llist) : unit =
      case l of
           ref Nil => ()
         | ref (Cons(x,xs)) => (filter p xs; if p x then () else l := !xs)

  (* append : 'a llist * 'a llist -> unit
   * REQUIRES: true
   * ENSURES: appends l2 to l1
   *)
  fun append (l1 : 'a llist, l2 : 'a llist) : unit =
      case l1 of
           ref Nil => l1 := !l2
         | ref (Cons(x,xs)) => append (xs, l2)

end

structure TestMutableList =
struct
  open MutableList
  (* Tests for tolist *)
  val example0 : int llist = ref Nil
  val example1 : int llist = ref Nil
  val example2 : int llist = ref (Cons (1, example1))
  val [1] = tolist example2
  val () = example1 := (Cons (2, ref Nil))
  val [1,2] = tolist example2
 
  (* Tests for map *) 
  val () = map (fn x => x) example0
  val test1 = ref (Cons (1, ref (Cons (2, ref (Cons (3, ref Nil))))))
  val () = map (fn x => x + 1) test1

  val [] = tolist example0
  val [2,3,4] = tolist test1

  (* Tests for filter *)
  val () = filter (fn x => true) example0
  val test1 = ref (Cons (1, ref (Cons (2, ref (Cons (3, ref Nil))))))
  val () = filter (fn x => (x mod 2) = 1) test1

  val [] = tolist example0
  val [1,3] = tolist test1

  (* Tests for append *)
  val test1 = ref (Cons (1, ref (Cons (2, ref (Cons (3, ref Nil))))))
  val test2end : int llist = ref Nil
  val test2 = ref (Cons (4, ref (Cons (5, ref (Cons (6, test2end))))))
  val () = append(test1,test2)

  val [1,2,3,4,5,6] = tolist test1
  val [4,5,6] = tolist test2

  val () = test2end := Cons (7, ref Nil)
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
val r = ref 0
datatype t = I of int
fun new () = (r := !r + 1; I(!r))
val eq = op =
end

structure RefNew = struct
type t = unit ref
val new : (unit -> t) = ref
val eq = op =
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

  fun init n = Seq.tabulate (fn i => ref (Var (N.new()))) n

  fun compress r =
      case r of
	  ref (Var i) => r
	| ref (Ptr r') => 
	  let val e = compress r'
	  in (r := Ptr e; e)
	  end

  fun unify s i j =
      let
	  val (r1, r2)   = (Seq.nth i s, Seq.nth j s)
	  val (r1', r2') = (compress r1, compress r2)
      in
	  r1' := Ptr r2'
      end

  fun eq s i j =
      let
	  val (ref (Var x), ref(Var y)) =
	      (compress (Seq.nth i s), compress (Seq.nth j s))
      in
	  N.eq(x,y)
      end
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
end


structure TestInt = TestIt(UnionFind(IntNew))
structure TestRef = TestIt(UnionFind(RefNew))
structure TestRef = TestIt(UnionFind(ExnNew))
