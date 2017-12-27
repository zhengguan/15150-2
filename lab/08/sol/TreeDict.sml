structure TreeDict : LABDICT=
struct
  (* Invariant: BST ordered on 'a (the keys) *)
  datatype ('a, 'b) tree =
    Leaf
  | Node of ('a, 'b) tree * ('a * 'b) * ('a, 'b) tree

  type ('a, 'b) dict = ('a, 'b) tree

  val empty = Leaf

  (* purpose: if k does not appear in any node of d, insert cmp d (k,v)
      evaluates to some d' such that d' is a BST according to cmp and k is
      bound to v.

      if k is bound to some v' in d, insert cmp d (k,v) evaluates to some
      d' such that d' is a BST according to cmp and k is bound to v.
   *)
  fun insert cmp d (k, v) =
    case d of
      Leaf => Node (empty, (k,v), empty)
    | Node (L, (k', v'), R) =>
      case cmp (k,k') of
        EQUAL => Node (L, (k, v), R)
      | LESS => Node (insert cmp L (k, v), (k', v'), R)
      | GREATER => Node (L, (k', v'), insert cmp R (k, v))

  val d1 = insert (Char.compare) empty (#"x",5)
  val d2 = insert (Char.compare) d1 (#"y",2)
  val Node(Leaf,(#"x",5),Leaf) = d1
  val Node(Leaf,(#"x",5),Node(Leaf,(#"y",2),Leaf)) = d2

  (* purpose: lookup cmp d k returns SOME(v) iff k is bound to v in d and d
        was built according to the ordering defined in cmp
   *)
  fun lookup cmp d k =
    case d of
      Leaf => NONE
    | Node (L, (k', v'), R) =>
      case cmp (k,k') of
        EQUAL => SOME v'
      | LESS => lookup cmp L k
      | GREATER => lookup cmp R k

  val SOME 5 = lookup (Char.compare) d1 #"x"
  val NONE = lookup (Char.compare) d2 #"z"

  fun modify cmp d (key,value) : ('k,'v) dict option =
  let
    fun mod' cmp Leaf key value = raise Fail"impossible"
      | mod' cmp (Node(l,(k,v),r)) key value =
        case cmp (key,k) of
             EQUAL => Node(l,(key, value),r)
            |LESS  => Node( (mod' cmp l key value), (k,v), r)
            |GREATER=> Node (l, (k,v), (mod' cmp r key value))
   in
    case lookup cmp d key of
         SOME x => SOME (mod' cmp d key value)
    |    NONE   => NONE
   end

  val SOME d = modify (Char.compare) d1 (#"x", 6)
  val SOME 6 = lookup (Char.compare) d #"x"
  val NONE = modify (Char.compare) d1 (#"t",32)



end
