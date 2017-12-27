structure BetterTreeDict : BETTERLABDICT=
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
      Leaf => Node (Leaf, (k,v), Leaf)
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
        lookup d k
    end

end


structure TestBetter = struct
  structure T = BetterTreeDict
  fun test() =
    let
      val d = T.empty (Char.compare)
      val d = T.insertWrapper d (#"a", 3)
      val d = T.insertWrapper d (#"b", 4)
      val d = T.insertWrapper d (#"c", 5)
      val SOME 3 = T.lookupWrapper d #"a"
      val NONE = T.lookupWrapper d #"d"
    in
      ()
    end
end

