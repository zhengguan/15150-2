functor SetOfSets (S : SET) : SET =
struct
    (* creating a structure to represent teh equality of sets *)
    structure SetEqual : EQUAL =
     struct
       type t = S.set

       (* equal : t * t -> bool
        * REQUIRES : true
        * ENSURES : equal (A,B) evaluates to true if sets A and B contain
        *     all of the same elements  *)
       fun equal (A : t,B : t) =
         S.void (S.union (S.difference A B) (S.difference B A))
     end

    structure SetDict = Dict(SetEqual)
    structure Power = DictSet(SetDict)

    open Power
end
