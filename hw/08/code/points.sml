structure Cartesian : POINT =
struct
    datatype point = Point of real * real

    fun realPairEq (a : real, b : real) (c : real, d : real) : bool =
        Real.== (a, c) andalso Real.== (b, d)

    (* eq : point -> point -> bool
     * REQUIRES: true
     * ENSURES: eq p1 p2 ==> true iff p1 and p2 represent the same point,
     *          else ==> false
     *)
    fun eq (p1 : point) (p2 : point) : bool =
        let
         val Point(x1, y1) = p1
         val Point(x2, y2) = p2
        in
          realPairEq (x1,y1)(x2,y2)
        end
    (* abs : point -> real
     * REQUIRES: true
     * ENSURES: abs p ==> the distance from p to the origin as a real number
     *)
    fun abs (p : point) : real =
        let
          val Point(x,y) = p
        in
          Math.sqrt((x*x) + (y*y))
        end
    (* dist : ponit -> point -> real
     * REQUIRES: true
     * ENSURES: dist p1 p2 ==> the distance between points p1 and p2
     *)
    fun dist (p1 : point) (p2 : point) : real =
        let
          val Point(x1, y1) = p1
          val Point(x2, y2) = p2
        in
          Math.sqrt(((x1-x2)*(x1-x2)) + ((y1-y2)*(y1-y2)))
        end

    (* ofCoords : (real * real) -> point
     * REQUIRES: true
     * ENSURES: ofCoords (x,y) ==> the point representing the
     *          Cartesian coordinates of x and y
     *)
    fun ofCoords (x : real, y : real) : point =
        Point(x,y)

    (* toCoords : point -> (real * real)
     * REQUIRES: true
     * ENSURES: toCoords p ==> (x,y), where x and y represent the
     * Cartesian coordinates represented by p
     *)
    fun toCoords (p : point) : (real * real) =
        let
          val Point(x,y) = p
        in
          (x,y)
        end
end

structure Polar : POINT =
struct
datatype point = Point of real * real

fun realPairEq (a : real, b : real) (c : real, d : real) : bool =
        Real.== (a, c) andalso Real.== (b, d)

(* easyRealEq : (real * real) -> (real * real) -> bool
 * REQUIRES: true
 * ENSURES: easyRealEq (a,b) (c,d) ==> true iff  (a,b) and (c,d)
 * represent the same ordered pairs of numbers
 *)
fun easyRealEq (a : real, b : real) (c : real, d: real) : bool =
    (Real.abs(a-c)<10E~5) andalso (Real.abs(b-d)<10E~5)

(* ofCoords : (real * real) -> point
 * REQUIRES: true
 * ENSURES: ofCoords (x,y) ==> the point representing
 *          the Cartesian coordinates given by (x,y)
 *)
fun ofCoords (x : real, y : real) : point =
    let
      val radius = Math.sqrt((x*x) + (y*y))
      val angle = Math.atan2(y,x)
    in
      Point(radius, angle)
    end

(* toCoords : point -> (real * real)
 * REQUIRES: true
 * ENSURES: toCoords p ==> the (x,y) pair representing the Cartesian
 *          coordinates given by p
 *)
fun toCoords (p : point) : (real * real) =
    let
      val Point(radius, angle) = p
    in
      (radius*(Math.cos(angle)),radius*(Math.sin(angle)))
    end

(* eq : point -> point -> bool
 * REQUIRES: true
 * ENSURES: eq p1 p2 ==> true iff p1 and p2 represent the same point, else
 *                   ==> false
 *)
fun eq (p1 : point) (p2 : point) : bool =
    let
      val (x1,y1) = toCoords p1
      val (x2,y2) = toCoords p2
    in
      easyRealEq (x1,y1)(x2,y2)
    end

(* abs : point -> real
 * REQUIRES: true
 * ENSURES: abs p ==> the distance from the point p to the coordinate (0,0)
 *)
fun abs (p : point) : real =
    let
      val Point(radius, _) = p
    in
      radius
    end

(* dist : point -> point -> real
 * REQUIRES : true
 * ENSURES: dist p1 p2 ==> the distance between the coordinates given by
 *          p1 and p2
 *)
fun dist (p1 : point) (p2 : point) : real =
    let
      val Point(r1, a1) = p1
      val Point(r2, a2) = p2
    in
      Math.sqrt((r1*r1)+(r2*r2)-(2.0*r1*r2*(Math.cos(a2-a1))))
    end

end

structure TestPoints =
struct
    structure C = Cartesian
    structure P = Polar
    fun realEq (a : real, b : real) : bool = Real.== (a, b)
    (* easyRealEq : (real * real) -> bool
     * REQUIRES: true
     * ENSURES: realEq (a,b) ==> true iff a = b
     *)
    fun easyRealEq (a : real, b : real) : bool =
        (Real.abs(a-b)<10E~5)
    val false = C.eq (C.ofCoords(3.5, 4.5)) (C.ofCoords(3.4, 4.4))
    val true = C.eq (C.ofCoords(12.0, 11.0))
               (C.ofCoords(C.toCoords(C.ofCoords(12.0, 11.0))))
    val true = realEq (C.abs (C.ofCoords(1.0,1.0)), Math.sqrt(2.0))
    val true = realEq (C.abs(C.ofCoords(~1.0, ~1.0)), Math.sqrt(2.0))
    val true = realEq ((C.dist(C.ofCoords(0.0,0.0))(C.ofCoords(5.0,0.0))),5.0)
    val true = P.eq (P.ofCoords(3.4, 4.5)) (P.ofCoords(3.4, 4.5))
    val false = P.eq (P.ofCoords(3.5, 4.5)) (P.ofCoords(3.4, 4.4))
    val true = P.eq (P.ofCoords(12.0, 11.0))
               (P.ofCoords(P.toCoords(P.ofCoords(12.0, 11.0))))
    val true = realEq (P.abs (P.ofCoords(1.0,1.0)), Math.sqrt(2.0))
    val true = realEq (P.abs(P.ofCoords(~1.0, ~1.0)), Math.sqrt(2.0))
    val true = realEq ((P.dist(P.ofCoords(0.0,0.0))(P.ofCoords(5.0,0.0))),5.0)
    val distP = P.dist(P.ofCoords(2.0,5.0)) (P.ofCoords(3.0,9.0))
    val distC = C.dist(C.ofCoords(2.0,5.0)) (C.ofCoords(3.0,9.0))
    val true = easyRealEq (distC, distP)
    val false = easyRealEq (distC, Math.sqrt(19.0))
    val true = easyRealEq ((C.abs(C.ofCoords(5.0,5.0))),
                           (P.abs(P.ofCoords(5.0,5.0))))

end
