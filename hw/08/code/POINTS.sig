signature POINT =
sig
  type point

  val eq : point -> point -> bool
  val abs : point -> real
  val dist : point -> point -> real
  val ofCoords: (real * real) -> point
  val toCoords: point -> (real * real)

end
