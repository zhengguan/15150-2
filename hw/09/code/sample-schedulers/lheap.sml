functor LHeap (Key : ORDERED) : LHEAP =
struct
  type key = Key.t
  datatype t = Empty
             | Node of { rank  : int
                       , left  : t
                       , value : key
                       , right : t }

  fun appendOrder EQUAL o2 = o2
    | appendOrder o1 _ = o1

  fun compare (l, r) =
      case (l, r) of
          (Empty, Empty) => EQUAL
        | (Empty, _) => LESS
        | (_, Empty) => GREATER
        | (Node {left = ll, value = lv, right = lr, ...},
           Node {left = rl, value = rv, right = rr, ...}) =>
          appendOrder (Key.compare (lv, rv))
                      (appendOrder (compare (ll, rl))
                                   (compare (rl, rr)))

  val empty = Empty

  fun rank Empty = 0
    | rank (Node {rank = r, ...}) = r

  fun mkNode l v r =
      case Int.compare (rank l, rank r) of
          LESS => Node { rank  = rank l + 1
                       , left  = r
                       , value = v
                       , right = l }
       | _ => Node { rank  = rank r + 1
                   , left  = l
                   , value = v
                   , right = r }

  fun single v = Node {rank = 1, left = Empty, value = v, right = Empty}

  fun merge Empty r = r
    | merge l Empty = l
    | merge (l as Node {left = ll, value = lv, right = lr, ...})
            (r as Node {left = rl, value = rv, right = rr, ...}) =
      case Key.compare (lv, rv) of
          LESS => mkNode ll lv (merge lr r)
        | _ => mkNode rl rv (merge l rr)

  fun insert v t = merge t (single v)

  fun head Empty = NONE
    | head (Node {value = v, ...}) = SOME v

  fun delete Empty = Empty
    | delete (Node {left = l, right = r, ...}) = merge l r

  fun pop t = case head t of
                  SOME x => SOME (x, delete t)
                | NONE => NONE
end

structure LHeapTests =
struct
  structure H = LHeap(struct type t = int; val compare = Int.compare end)

  val NONE = H.head H.empty
  val EQUAL = H.compare (H.empty, H.delete H.empty)

  val theap = H.insert 1 (H.insert 4 (H.insert 0 H.empty))
  val SOME 0 = H.head theap
  val SOME 1 = H.head (H.delete theap)
  val SOME 4 = H.head (H.delete (H.delete theap))
end
