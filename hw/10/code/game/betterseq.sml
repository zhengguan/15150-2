functor BetterSeq(Seq : SEQUENCE) : BETTERSEQ =
struct

open Seq

fun insert (x : 'a) (S : 'a seq) (i : int) =
    tabulate (fn ind => if ind = i then x else nth ind S) (length S)
    
fun mapIdx (f : 'a * int -> 'b) (S : 'a seq) : 'b seq =
    tabulate (fn i => f(nth i S, i)) (length S)

fun extractSomes (S : 'a option seq) : 'a seq =
    map (fn SOME x => x | NONE => raise Fail "Filter Broke?")
        (filter (fn NONE => false | _ => true) S)

end
