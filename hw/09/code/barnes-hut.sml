functor BarnesHut (A : BHArgs) : BARNESHUT =
struct

  structure Args = A

  (* helpful aliases to clean up the code below *)
  structure Seq = A.BB.Seq
  structure Plane = A.BB.Plane
  structure BB = A.BB
  structure Mech = A.Mech
  structure Scalar = A.BB.Plane.Scalar
  open A.Mech

  val ++ = Plane.++
  val ** = Plane.**
  val --> = Plane.-->
  val // = Plane.//

  infixr 3 ++
  infixr 4 **
  infixr 3 -->
  infixr 3 //

  (* this is the core idea of the barnes-hut algorithm *)
  (* a bhtree is a finitely branching tree of bodies that also
   * stores the center of mass and diameter of the bounding box
   * containing its children *)
  datatype bhtree =
      Null
    | Singleton of body
    | Box of (Plane.scalar * Plane.point) * Plane.scalar * (bhtree Seq.seq)
      (* ((mass, center), box diameter, sequence of length four
                                        representing each quadrant) *)

  exception Unimplemented

  (* scale_point : Scalar.scalar * Plane.point -> Plane.vec*)
  (* ENSURES: Scales the vector from the origin to p by the factor m. *)
  fun scale_point (m : Scalar.scalar, p : Plane.point) : Plane.vec =
      (Plane.origin --> p) ** m

  (* Task 4.1 *)
  (* barycenter : (Scalar.scalar * Plane.point) Seq.seq ->
                  Scalar.scalar * Plane.point
   * REQUIRES: s is a non-empty sequence of mass and position pairs
               representing bodies in a pseudobody
   * ENSURES: barycenter s ==> (m,c) where m is the total mass of the bodies
   *          and c is the barycenter
   *)
  fun barycenter (s : (Scalar.scalar * Plane.point) Seq.seq) :
      Scalar.scalar * Plane.point =
      let
        val sum1 = Plane.sum scale_point s
        val sum2 = Seq.reduce Scalar.plus Scalar.zero (Seq.map
                                                           (fn (m,p) => m) s)
      in
        (sum2, (Plane.displace(Plane.origin, ((sum1)//sum2))))
      end

  (* Testing hint: use seqFromList and seqEq and boxEq
     to make at least one test case.
     You may find the boxes defined above to be helpful. *)

  (* Task 4.2 *)
  (* quadrantize : BB.box -> body Seq.seq -> body Seq.seq Seq.seq
   * REQUIRES: the positions of the bodies in s must lie within the
   *           bounding box given by b
   * ENSURES: quadrantize b s ==> a sequences of sub-sequences of bodies
   *          where the bodies in the ith sub-sequence are those that lie
   *          in the ith quadrant of b
   *)
  fun quadrantize (b : BB.box) (s : body Seq.seq) : body Seq.seq Seq.seq =
     Seq.tabulate (fn i =>
                      Seq.filter (fn b' : body =>
                                     (BB.quadrant b (position b')) = i) s) 4

  (* center_of_mass bhtree -> Scalar.scalar * Plane.point *)
  (* ENSURES
   * Projects the mass and center from the root node of a bhtree *)
  fun center_of_mass (T : bhtree) : Scalar.scalar * Plane.point =
      case T of
          Null => (Scalar.zero, Plane.origin)
        | Singleton (m, p, _) => (m, p)
        | Box (com, _, _) => com

  (* Task 4.3 *)
  (* compute_tree : body Seq.seq -> bhtree
   * REQUIRES: no two bodies in s have the same position
   * ENSURES: compute_tree s ==> T, where T is the BH tree decomposition of s
   *)
  fun compute_tree (s : body Seq.seq) : bhtree =
     case (Seq.length s) of
       0 => Null
      |1 => Singleton (Seq.nth 0 s)
      |_ =>
       let
         val pSeq = Seq.map position s
         val b = BB.hull pSeq
         val d = BB.diameter b
         val qSeq = quadrantize b s
         val bSeq = Seq.map compute_tree qSeq
         val (m, c) = barycenter (Seq.map center_of_mass bSeq)
       in
         Box((m,c), d, bSeq)
       end

  (* Testing hint: write at least one test case by
     working out the answer for (compute_tree bseq bb4).
     *)

  (* Task 4.4 *)
  (* groupable : Plane.point -> Plane.point -> Plane.scalar -> Scalar.scalar ->
                 bool
   * REQUIRES: t > 0, p1 and p2 are not equivalent points
   * ENSURES: groupable p1 p2 bd t ==> true if bd/r <= t, where r is the
   *          distance between p1 and p2, and
   *                               ==> false otherwise
   *)
  fun groupable (p1 : Plane.point) (p2 : Plane.point) (bd : Plane.scalar)
              (t : Scalar.scalar) : bool =
      case Scalar.compare(Scalar.divide(bd,(Plane.distance p1 p2)),t) of
        GREATER => false
       |_ => true

  (* Task 3.5 *)
  (* bh_acceleration : bhtree -> Scalar.scalar -> body -> Plane.vec
   * REQUIRES: t > 0
   * ENSURES: bh_acceleration T t b ==> the vector representing
   *          b's aceleration due to the bodies in T according to the
   *          Barnes-Hut algorithm using the threshold t
   *)
  fun bh_acceleration (T : bhtree) (t : Scalar.scalar) (b as (_, p, _))
      : Plane.vec =
      case T of
        Null => Plane.zero
       |Singleton (m', p', _) => accOnPoint (p,(m', p'))
       |Box((m,c), bd, Ts) =>
        (case (groupable (position b) c bd t) of
           true  => accOnPoint (p,(m, c))
          |false => Plane.sum (fn T' => bh_acceleration T' t b) Ts)

  (*
     barnes_hut : Plane.scalar -> body Seq.seq -> Plane.vec Seq.seq

   * ENSURES
     Given a threshold and a sequence of bodies, compute the acceleration
     on each body using the Barnes-Hut algorithm.
   *)
  fun barnes_hut (threshold : Plane.scalar) (s : body Seq.seq)
      : Plane.vec Seq.seq =
      Seq.map (bh_acceleration (compute_tree s) threshold) s

  val accelerations : body Seq.seq -> Plane.vec Seq.seq =
      barnes_hut (Plane.Scalar.fromRatio (A.thresh))
end

structure RatTrans = Transcripts(BarnesHut(RatArgs))
structure RealTrans = Transcripts(BarnesHut(RealArgs))
