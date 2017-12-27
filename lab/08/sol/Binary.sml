structure Binary : ARITHMETIC =
struct
  type digit = int (* uses 0, 1 *)
  type integer = digit list

  fun rep 0 = []
    | rep n = (n mod 2) :: rep (n div 2)

  (* carry : digit * integer -> integer *)
  fun carry (0, ps) = ps
    | carry (c, []) = [c]
    | carry (c, p :: ps) =
      ((p + c) mod 2) :: carry ((p + c) div 2, ps)

  fun add ([], qs) = qs
    | add (ps, []) = ps
    | add (p :: ps, q :: qs) =
          ((p + q) mod 2) :: carry ((p + q) div 2, add (ps, qs))

  fun mult  ([], _) = []
    | mult (_, []) = []
    | mult (0 :: ps, qs) = 0 :: mult (ps, qs)
    | mult (p :: ps, qs) = add (qs, 0 :: mult (ps, qs))

  fun display L = foldl (fn (d, s) => Int.toString d ^ s) "" L

  fun toInt [] = 0
    | toInt (p :: ps) = p + 2 * toInt ps

end

structure TestBinary =
struct
  open Binary

  val 0 = toInt (rep 0)
  val 1 = toInt (rep 1)
  val 2 = toInt (rep 2)
  val 7 = toInt (rep 7)

  val 0 = toInt (add (rep 0, rep 0))
  val 10 = toInt (add (rep 0, rep 10))
  val 10 = toInt (add (rep 10, rep 0))
  val 65 = toInt (add (rep 35, rep 30))

  val 0 = toInt (mult (rep 0, rep 0))
  val 0 = toInt (mult (rep 0, rep 10))
  val 0 = toInt (mult (rep 10, rep 0))
  val 30 = toInt (mult (rep 5, rep 6))
  val 30 = toInt (mult (rep 6, rep 5))

  val "" = display (rep 0)
  val "1" = display (rep 1)
  val "10" = display (rep 2)
  val "101010" = display (rep 42)
end
