structure TreeDict : BETTERLABDICT=
struct
  (* Invariant: BST ordered on 'a (the keys) *)
  datatype ('a, 'b) tree =
    Leaf
  | Node of ('a, 'b) tree * ('a * 'b) * ('a, 'b) tree

  type ('a, 'b) dict = (('a, 'b) tree * ('a*'a -> order))

  val empty = (fn cmp => (Leaf, cmp))

  (* purpose: if k does not appear in any node of d, insert cmp d (k,v)
      evaluates to some d' such that d' is a BST according to cmp and k is
      bound to v.

      if k is bound to some v' in d, insert cmp d (k,v) evaluates to some
      d' such that d' is a BST according to cmp and k is bound to v.
   *)
  fun insertWrapper (d,cmp) (k, v) =
  let fun insert cmp d (k,v) =
    case d of
      Leaf => Node (empty, (k,v), empty)
    | Node (L, (k', v'), R) =>
      case cmp (k,k') of
        EQUAL => (Node (L, (k, v), R))
      | LESS => Node (insert cmp L (k, v), (k', v'), R)
      | GREATER => Node (L, (k', v'), insert cmp R (k, v))
    in
      (insert cmp d (k,v), cmp)
    end


  (* purpose: lookup cmp d k returns SOME(v) iff k is bound to v in d and d
        was built according to the ordering defined in cmp
   *)
  fun lookupWrapper D k =
  let
    val (d,cmp) = D
    fun lookup d k =
    case d of
      Leaf => NONE
    | Node (L, (k', v'), R) =>
      case cmp (k,k') of
        EQUAL => SOME v'
      | LESS => lookup L k
      | GREATER => lookup R k
    in
      (lookup  d k , cmp)
    end




end
