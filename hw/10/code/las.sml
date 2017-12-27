structure LookAndSay =
struct
  structure Seq = BetterSeq(Seq)

  (*l_a_sHelp : ('a * 'a -> bool) -> (int * 'a) -> ('a Seq.seq) ->
                (int * 'a) Seq.seq
   * REQUIRES: f is total, c is the current number of instances of x
   *           encountered so far in the left-to-right traversal of the
   *           sequence
   *)
   fun l_a_sHelp (f : 'a * 'a -> bool) (c : int, x : 'a) (S : 'a Seq.seq) :
                    ((int * 'a) Seq.seq) =
       case (Seq.showl S) of
         Seq.Nil => Seq.singleton((c,x))
        |Seq.Cons(y, S') => (case (f(x,y)) of
                               true  => (l_a_sHelp f ((c+1),x) S')
                              |false => (Seq.cons (c,x)
                                        (l_a_sHelp f (1,y) S')))
   val test1 = Seq.fromList [(3,2)]
   val test2 = Seq.fromList [(500,6)]
   val true = ((Seq.nth 0 test1) =
               (Seq.nth 0 (l_a_sHelp op= (1,2) (Seq.fromList([2,2])))))
   val true = ((Seq.nth 0 test2) =
               (Seq.nth 0 (l_a_sHelp op= (500,6) (Seq.empty ()))))

  (*look_and_say: ('a * 'a -> bool) -> 'a Seq.seq -> (int *'a) Seq.seq
   *REQUIRES: f is total
   *ENSURES: look_and_say f S => a look-and-say version of S, e.g.
   *         look_and_say streq <"hi", "hi", "hi", "bye", "bye"> ==>
   *                            <(3,"hi"),(2,"bye")>
   *)
   fun look_and_say (f : 'a * 'a -> bool) (S : 'a Seq.seq) :
     ((int * 'a) Seq.seq) =
       case (Seq.showl S) of
         Seq.Nil => (Seq.empty ())
        |Seq.Cons(x : 'a, S' : 'a Seq.seq) => l_a_sHelp f (1,x) S'

   val true = ((Seq.nth 0 test1) =
               (Seq.nth 0 (look_and_say op= (Seq.fromList([2,2,2])))))
   val test3 = Seq.fromList [(2, "hi"), (3, "bye")]
   val true = ((Seq.nth 0 test3) =
               (Seq.nth 0 (look_and_say op= (Seq.fromList["hi","hi",
                                                         "bye","bye","bye"]))))
   val true = ((Seq.nth 1 test3) =
               (Seq.nth 1 (look_and_say op= (Seq.fromList["hi","hi",
                                                         "bye","bye","bye"]))))


  (*las_wrap: ('a * 'a -> bool) -> 'a Seq.seq -> (int * 'a) Seq.seq
   *REQUIRES: f is total
   *ENSURES: las_wrap f S => a look-and-say version of S interpreted as a
   *         cycle, as if the last element of S wrapped around such that
   *         the element after the "last" is the first, e.g.
   *         las_wrap streq <"bye","hi","hi","bye"> =>
   *                        <(2,"bye"),(2,"hi")>
   *)
   fun las_wrap (f : 'a * 'a -> bool) (S : 'a Seq.seq) :
     ((int * 'a) Seq.seq) =
       let
          val las = look_and_say f S
          val l = Seq.length las
       in
          (case (Seq.showl las) of
             Seq.Nil => (Seq.empty ())
            |Seq.Cons((c1,x1),_) =>
             (let
                 val (c2, x2) = Seq.nth (l-1) las
              in
                 (case (f(x1,x2)) of
                    false => las
                   |true => Seq.cons ((c1+c2),x1) (Seq.tabulate (fn i =>
                                                  (Seq.nth (i+1) las)) (l-2)))
              end))
       end

val true = ((Seq.nth 1 test3) =
               (Seq.nth 1 (las_wrap  op= (Seq.fromList["hi","hi",
                                                         "bye","bye","bye"]))))
val test4 = Seq.fromList [(2,"bye"),(2,"hi")]
val true = ((Seq.nth 0 test4) =
               (Seq.nth 0 (las_wrap op= (Seq.fromList["bye",
                                                         "hi","hi","bye"]))))
val true = ((Seq.nth 1 test4) =
               (Seq.nth 1 (las_wrap op= (Seq.fromList["bye",
                                                         "hi","hi","bye"]))))

end
