functor PairOfEqual (PE : PAIR_OF_EQUAL) : EQUAL = struct
  open PE

  type t = E1.t * E2.t

  (* equal : t * t -> bool
   * REQUIRES: true
   * ENSURES: E1, E2 equality comparisons will both eval to true on result *)
  fun equal ((x1, y1), (x2, y2)) =
    E1.equal (x1, x2) andalso E2.equal (y1, y2)
end

