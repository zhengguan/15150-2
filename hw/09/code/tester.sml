functor TestUtil (BH : BARNESHUT) =
struct
  structure A = BH.Args

  open A.Mech
  structure Seq = A.BB.Seq
  structure P = A.BB.Plane
  structure BB = A.BB
  structure Mech = A.Mech
  structure Scalar = A.BB.Plane.Scalar

  val ++ = Plane.++
  val ** = Plane.**
  val --> = Plane.-->
  val // = Plane.//

  infixr 3 ++
  infixr 4 **
  infixr 3 -->
  infixr 3 //

  fun seqFromList (l : 'a list) : 'a Seq.seq =
      List.foldr (fn (x,y) => Seq.cons x y) (Seq.empty()) l

  fun seqAll (f : 'a -> bool) (s : 'a Seq.seq) : bool =
      Seq.mapreduce f true (fn (b1, b2) => b1 andalso b2) s

  fun seqEq (aEq : 'a * 'a -> bool) (s1 : 'a Seq.seq) (s2 : 'a Seq.seq) : bool =
      Seq.length s1 = Seq.length s2 andalso (seqAll aEq (Seq.zip (s1, s2)))

  fun boxEq (bb1, bb2) : bool =
      seqEq Plane.pointEqual (BB.vertices bb1) (BB.vertices bb2)

  (* Note: Doesn't compare velocities as these are unaffected by compute_tree *)
  fun bodyEq ((m1, p1, _) : body, (m2, p2, _) : body) : bool =
      (Scalar.eq (m1, m2)) andalso Plane.pointEqual (p1, p2)

  fun bhtreeEq (t1 : BH.bhtree, t2 : BH.bhtree) : bool =
      case (t1, t2) of
        (BH.Null, BH.Null) => true
      | (BH.Singleton b1, BH.Singleton b2) => bodyEq (b1, b2)
      | (BH.Box ((cm1, cp1), bd1, ts1), BH.Box ((cm2, cp2), bd2, ts2)) =>
        Scalar.eq (cm1, cm2) andalso Plane.pointEqual (cp1, cp2)
        andalso Scalar.eq (bd1, bd2) andalso seqEq bhtreeEq ts1 ts2
      | (_,_) => false

  (* some points and bounding boxes and bodies to use for testing *)
  val p00 = Plane.origin
  val p44 = Plane.fromcoord (Scalar.fromInt 4, Scalar.fromInt 4)
  val p02 = Plane.fromcoord (Scalar.zero, Scalar.fromInt 2)
  val p24 = Plane.fromcoord (Scalar.fromInt 2, Scalar.fromInt 4)
  val p22 = Plane.fromcoord (Scalar.fromInt 2, Scalar.fromInt 2)
  val p20 = Plane.fromcoord (Scalar.fromInt 2, Scalar.zero)
  val p42 = Plane.fromcoord (Scalar.fromInt 4, Scalar.fromInt 2)
  val p01 = Plane.fromcoord (Scalar.fromInt 0, Scalar.fromInt 1)
  val p11 = Plane.fromcoord (Scalar.fromInt 1, Scalar.fromInt 1)
  val p40 = Plane.fromcoord (Scalar.fromInt 4, Scalar.zero)
  val p04 = Plane.fromcoord (Scalar.zero, Scalar.fromInt 4)
  val p13 = Plane.fromcoord (Scalar.one, Scalar.fromInt 3)
  val p33 = Plane.fromcoord (Scalar.fromInt 3, Scalar.fromInt 3)
  val p31 = Plane.fromcoord (Scalar.fromInt 3, Scalar.fromInt 1)

  val bb0 : BB.box = BB.fromPoints (p02,p24)
  val bb1 : BB.box = BB.fromPoints (p22,p44)
  val bb2 : BB.box = BB.fromPoints (p00,p22)
  val bb3 : BB.box = BB.fromPoints (p20,p42)
  val bb4 : BB.box = BB.fromPoints (p00,p44)
  val bb5 : BB.box = BB.fromPoints (p00,p33)

  val body1 : body = (Scalar.one, p40, Plane.zero)
  val body2 : body = (Scalar.one, p22, Plane.zero)
  val body3 : body = (Scalar.one, p04, Plane.zero)
  val body13 : body = (Scalar.one, p13, Plane.zero)
  val body31 : body = (Scalar.one, p31, Plane.zero)
  val body11 : body = (Scalar.one, p11, Plane.zero)
  val body33 : body = (Scalar.one, p33, Plane.zero)

end

functor Tester (BH : BARNESHUT) =
struct
  structure Util = TestUtil(BH)
  structure Seq = Util.Seq
  structure Scalar = Util.Scalar
  structure Plane = Util.P
  structure BB = Util.BB

  (* sample tests using values in Util *)
  val false = Util.boxEq (Util.bb0, Util.bb1)
  val true = Util.bhtreeEq (BH.Null, BH.Null)
  val true = (let
                val seq1 = Seq.fromList([(Scalar.fromInt(1),Util.p44),
                                         (Scalar.fromInt(5),Util.p24)])
                val (m, c) = BH.barycenter seq1
                val (s1, s2) = (Scalar.fromInt(14), Scalar.fromInt(6))
                val x = Scalar.divide(s1,s2)
                val y = Scalar.fromInt(4)
                val p = Plane.fromcoord(x,y)
              in
               (Scalar.eq(Scalar.fromInt(6),m)) andalso (Plane.pointEqual(p,c))
              end)
  val true = (let
                val seq2 = Seq.fromList([(Scalar.fromInt(7),Util.p33),
                                         (Scalar.fromInt(9),Util.p40),
                                         (Scalar.fromInt(2),Util.p42)])
                val (m, c) = BH.barycenter seq2
                val (s1, s2, s3) = (Scalar.fromInt(65),
                                    Scalar.fromInt(25),
                                    Scalar.fromInt(18))
                val x = Scalar.divide(s1,s3)
                val y = Scalar.divide(s2,s3)
                val p = Plane.fromcoord(x,y)
              in
              (Scalar.eq(Scalar.fromInt(18),m)) andalso (Plane.pointEqual(p,c))
              end)
  val seq1 = Seq.tabulate (fn i => case i of
                              0 => Util.body13
                             |1 => Util.body33
                             |2 => Util.body11
                             |3 => Util.body31
                             |_ => Util.body1) 5
  val seqSeq1 = Seq.tabulate (fn i => case i of
                              0 => (Seq.tabulate (fn i => Util.body13) 1)
                             |1 => (Seq.tabulate (fn i => Util.body33) 1)
                             |2 => (Seq.tabulate (fn i => Util.body11) 1)
                             |_ => (Seq.tabulate (fn i => case i of
                                                          0 => Util.body31
                                                         |_ => Util.body1) 2)) 4
  val true = Util.bodyEq((Seq.nth 0 (Seq.nth 0 seqSeq1)),
                         (Seq.nth 0 (Seq.nth 2 (BH.quadrantize Util.bb4 seq1))))
  val true = Util.bodyEq((Seq.nth 1 (Seq.nth 3 seqSeq1)),
                         (Seq.nth 1 (Seq.nth 1 (BH.quadrantize Util.bb4 seq1))))
  val true = Util.bodyEq((Seq.nth 0 (Seq.nth 2 seqSeq1)),
                         (Seq.nth 0 (Seq.nth 0 (BH.quadrantize Util.bb4 seq1))))
  val seq2 = Seq.tabulate (fn i => case i of
                              0 => Util.body13
                             |1 => Util.body33
                             |2 => Util.body11
                             |_ => Util.body31) 4
  val tree1 = BH.compute_tree seq2
  val tree2 = (let
                 val (s1, s2) = (Scalar.fromInt(8),
                                 Scalar.fromInt(4))
                 val x = Scalar.divide(s1,s2)
                 val y = Scalar.divide(s1,s2)
                 val p = Plane.fromcoord(x,y)
                 val d = BB.diameter Util.bb2
               in
                 BH.Box((s2,p),d,Seq.fromList(
                [(BH.Singleton Util.body11),
                 (BH.Singleton Util.body31),
                 (BH.Singleton Util.body13),
                 (BH.Singleton Util.body33)]))
               end)
  val true = BH.groupable Util.p00 Util.p44
                          (Scalar.fromInt(4)) (Scalar.fromInt(1))
  val true = Util.bhtreeEq(tree1, tree2)
end

(* run our tests on rationals since they're reliable *)
structure RatTests = Tester(BarnesHut(RatArgs))
