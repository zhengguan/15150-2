structure Rational :> RATIONAL =
struct
   type t = int * int

   val negate = fn ((n, d) : t) : t => (~n, d)

   fun gcd (m  : int, n : int) : int =
       case (m , n) of
           (m , 0) => m
         | (0 , n) => n
         | (m , n) => (case m > n of
                           true => gcd (m mod n , n)
                         | false => gcd (m , n mod m))

   fun lcm (m : int, n : int) : int =
       let
           val g = gcd (m, n)
       in
           m * n div g
       end

   fun inverse ((n, d) : t) : t =
       case Int.compare (n, 0) of
           LESS => (~d, ~n)
         | EQUAL => raise Fail "inverse of zero"
         | GREATER => (d, n)

   fun fromPair (n : int, d : int) : t =
       let
         val g = case (Int.compare (n, 0), Int.compare (d, 0)) of
                     (_, EQUAL) => raise Fail "denominator can't be zero"
                   | (EQUAL, _) => d
                   | (LESS, GREATER) => gcd (~n, d)
                   | (GREATER, LESS) => ~(gcd (n, ~d))
                   | (LESS, LESS) => ~(gcd (~n, ~d))
                   | (GREATER, GREATER) => gcd (n, d)
       in
         (n div g, d div g)
       end

   fun fromInt (n) : t = fromPair(n,1)

   fun plus ((n1, d1) : t, (n2, d2) : t) : t =
       let
         val cdenom = lcm (d1, d2)
       in
         (n1 * cdenom div d1 + n2 * cdenom div d2, cdenom)
       end

   fun times ((n1, d1) : t, (n2, d2) : t) : t =
       fromPair (n1 * n2, d1 * d2)

   fun divide (r1 : t, r2 : t) : t = times (r1, inverse r2)

   fun subtract (r1 : t, r2 : t) : t = plus (r1, negate r2)

   fun toString ((n, d) : t) : string =
       case d of
           1 => Int.toString n
         | _ => Int.toString n ^ "/" ^ Int.toString d

   fun compare ((n1, d1) : t, (n2, d2) : t) : order =
       let
         val cdenom = lcm (d1, d2)
       in
           Int.compare (n1 * cdenom div d1, n2 * cdenom div d2)
       end
end
