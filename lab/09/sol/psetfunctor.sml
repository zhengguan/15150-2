functor PowerSetFunctor (S : SET) : POWERSETFUNCTOR =
struct
    structure P = SetOfSets(S)
    type elem = S.elem
    type set = S.set
    type powerset = P.set

    (* insert : elem * elem list list -> elem list list *)
    fun insert (e : elem, [] : elem list list) : elem list list = []
      | insert (e, l::ls) = (e::l) :: insert(e, ls)

    (* poserList :  elem list -> elem list list *)
    fun powerList ([]: elem list) : elem list list  = [[]]
      | powerList (x::xs) = powerList(xs) @ insert(x, powerList(xs))


    (* make_powerset : set -> powerset *)
    fun make_powerset (s : set) : powerset =
        let
            val subsets = map S.fromList (powerList (S.toList s))
        in
            P.fromList subsets
        end

    (* make_powerfun : (elem -> elem) -> (set -> set) *)
    fun make_powerfun (f : elem -> elem) : (set -> set) =
        fn s => S.fromList (map f (S.toList s))
end

