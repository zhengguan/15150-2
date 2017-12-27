structure Decimal : ARITHMETIC =
struct
  type digit = int (* uses 0 through 9 *)
  type integer = digit list
    
  fun rep 0 = []
    | rep n = (n mod 10) :: rep (n div 10)

  (* carry : digit * integer -> integer *)
  fun carry (0, ps) = ps
    | carry (c, []) = [c]
    | carry (c, p :: ps) =
      ((p + c) mod 10) :: carry ((p + c) div 10, ps)

  fun add ([], qs) = qs
    | add (ps, []) = ps
    | add (p :: ps, q :: qs) = 
      ((p + q) mod 10) :: carry ((p + q) div 10, add (ps, qs))

  (* times : digit -> integer -> integer *)
  fun times (0, qs) = []
    | times (k, []) = []
    | times (k, q :: qs) = 
      ((k * q) mod 10) :: carry ((k * q) div 10, times (k, qs))

  fun mult ([], _) = []
    | mult (_, []) = []
    | mult (p :: ps, qs) = add (times (p, qs), 0 :: mult (ps, qs))
  
  fun display L = foldl (fn (d, s) => Int.toString d ^ s) "" L
  
  fun toInt [] = 0
    | toInt (p :: ps) = p + 10 * toInt ps

end

structure TestDecimal =
struct
  open Decimal
  
  val 0 = toInt (rep 0)
  val 1 = toInt (rep 1)
  val 2 = toInt (rep 2)
  val 7 = toInt (rep 7)
  val 9 = toInt (rep 9)
  val 9912 = toInt (rep 9912)

  val 0 = toInt (add (rep 0, rep 0))
  val 10 = toInt (add (rep 0, rep 10))
  val 10 = toInt (add (rep 10, rep 0))
  val 65 = toInt (add (rep 35, rep 30))
  
  val 0 = toInt (mult (rep 0, rep 0))
  val 0 = toInt (mult (rep 0, rep 10))
  val 0 = toInt (mult (rep 10, rep 0))
  val 30 = toInt (mult (rep 5, rep 6))
  val 30 = toInt (mult (rep 6, rep 5))
  val 630 = toInt (mult (rep 42, rep 15))

  val "" = display (rep 0)
  val "1" = display (rep 1)
  val "2" = display (rep 2)
  val "42" = display (rep 42)
end