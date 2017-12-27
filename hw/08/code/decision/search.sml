structure Search : SEARCH =
struct
  (*structure P = Tree*)

  (* also try testing with one of these once you have implemented them:*)
  (*structure P = Shrub*)
  structure P = Combinations

  (* find_help : P.elem -> P.stree -> (P.result -> 'a) -> (unit -> 'a) -> 'a
   * REQUIRES: s is total
   * ENSURES: find_help i t s k ==> s(r), where r is the list of decisions
   * that leads to finding i in t, or k() if i is not in t
   *)
  fun find_help (i : P.elem) (t : P.stree) (s : P.result -> 'a)
      (k : unit -> 'a) : 'a =
      case (P.atRoot(i,t)) of
        true  => s []
       |false => (case (P.branch t) of
                    [] => k()
                   |(d, st)::xs =>
                    find_help i st (fn r => s(d::r)) (fn () =>
                    foldr (fn ((d',st'),s') =>
                              find_help i st' (fn r'' => s(d'::r''))
                                              (fn () => s')) (k()) xs))


  fun find (i : P.elem) (t : P.stree) =
      find_help i t SOME (fn () => NONE)

(* Combinations tests: tried and true *)

val SOME[P.Mult 5, P.Subtract 9] = find 26 ([7,5,9])
val NONE = find 0 []
val SOME([P.Mult 0, P.Add 11]) = find 11 ([6,0,11])
val SOME([P.Subtract 5]) = find 2 [7,5,9]
val NONE = find 40 ([7,6,0])
val SOME([P.Add ~1]) = find 0 [1, ~1, 0]
val SOME([P.Subtract ~1]) = find 2 [1, ~1, 0]
val SOME([P.Mult 6]) = find 42 [7,6,0]
val SOME([P.Subtract 5, P.Add 2]) = find 4 [7,5,2]
val SOME([P.Add 6]) = find 13 [7,6,0]

(* Shrub structure tests: they work, I swear!*)
(*
val SOME([P.Center,P.Right]) = find 6 (P.Node(
                                       P.Node(P.Leaf 1, P.Leaf 2, P.Leaf 3),
                                       P.Node(P.Leaf 4, P.Leaf 5, P.Leaf 6),
                                       P.Node(P.Leaf 7, P.Leaf 8, P.Leaf 9)))
val SOME([P.Left,P.Center]) = find 2 (P.Node(
                                       P.Node(P.Leaf 1, P.Leaf 2, P.Leaf 3),
                                       P.Node(P.Leaf 4, P.Leaf 5, P.Leaf 6),
                                       P.Node(P.Leaf 7, P.Leaf 8, P.Leaf 9)))
val NONE = find 42 (P.Node(P.Leaf 40, P.Leaf 41, P.Leaf 43))
val NONE = find 0 P.Empty
val SOME([P.Center, P.Left]) = find 9 (P.Node(
                     P.Node(P.Empty, P.Leaf 7, P.Leaf 1100),
                     P.Node(P.Leaf 9, P.Empty, P.Leaf 3),
                     P.Node(P.Leaf 50, P.Empty, P.Empty)))
val SOME([]) = find 42 (P.Leaf 42)
val SOME([P.Right]) = find ~1 (P.Node(P.Leaf 5, P.Leaf 100, P.Leaf ~1))
*)

(* Tree structure tests: these work too, I swear!

  val SOME([P.Right,
            P.Right]) = find 5 (P.Node(P.Node(P.Node(P.Empty,3,P.Empty),89,
                                     P.Node(P.Empty,2,P.Empty)),
                            75,
                              P.Node(P.Node(P.Empty,4,P.Empty),69,
                                     P.Node(P.Empty,5,P.Empty))))
  val SOME([P.Right,
           P.Right,
           P.Left]) = find ~12 (P.Node(P.Node(P.Node(P.Node(
                                                      P.Empty,~91,P.Empty),
                                                      ~7,
                                                      P.Node(
                                                      P.Empty,101,P.Empty)),
                                               6,
                                               P.Node(P.Node(
                                                      P.Empty,4,P.Empty),
                                                     13,
                                                     P.Node(
                                                     P.Empty,19,P.Empty))),
                                       5,
                                       P.Node(P.Node(P.Node(
                                                     P.Empty,8,P.Empty),
                                                     12,
                                                     P.Node(
                                                     P.Empty,7,P.Empty)),
                                              10,
                                              P.Node(P.Node(
                                                     P.Empty,~12,P.Empty),
                                                     26,
                                                     P.Node(
                                                     P.Empty,29,P.Empty)))))

  val NONE  = find ~12 (P.Node(P.Node(P.Node(P.Node(
                                                      P.Empty,~91,P.Empty),
                                                      ~7,
                                                      P.Node(
                                                      P.Empty,101,P.Empty)),
                                               6,
                                               P.Node(P.Node(
                                                      P.Empty,4,P.Empty),
                                                     13,
                                                     P.Node(
                                                     P.Empty,19,P.Empty))),
                                       5,
                                       P.Node(P.Node(P.Node(
                                                     P.Empty,8,P.Empty),
                                                     12,
                                                     P.Node(
                                                     P.Empty,7,P.Empty)),
                                              10,
                                              P.Node(P.Node(
                                                     P.Empty,~13,P.Empty),
                                                     26,
                                                     P.Node(
                                                     P.Empty,29,P.Empty)))))
*)
end
