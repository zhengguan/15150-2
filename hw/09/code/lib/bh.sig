signature BARNESHUT = sig
  structure Args : BHArgs
  structure BB : BOX = Args.BB
  structure Mech : MECHANICS = Args.Mech
  structure Plane : SPACE = BB.Plane
  structure Seq : SEQUENCE = BB.Seq

  (* this is the core idea of the barnes-hut algorithm *)
  (* a bhtree is a finitely branching tree of bodies that also
   * stores the center of mass and bounding box containing it's
   * children *)
  datatype bhtree =
      Null
    | Singleton of Mech.body
    | Box of (Plane.scalar * Plane.point) * Plane.scalar * (bhtree Seq.seq)
      (* ((mass, center), box diameter, quadrants*)

  val barycenter : (Plane.scalar * Plane.point) Seq.seq -> Plane.scalar * Plane.point
  val quadrantize : BB.box -> Mech.body Seq.seq -> Mech.body Seq.seq Seq.seq
  val compute_tree : Mech.body Seq.seq -> bhtree
  val groupable : Plane.point -> Plane.point -> Plane.scalar -> Plane.scalar -> bool

  val accelerations : Mech.body Seq.seq -> BB.Plane.vec Seq.seq
end
