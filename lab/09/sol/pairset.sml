functor PairSet (PE: PAIR_OF_EQUAL) : SET =
struct

  structure CartEqual = PairOfEqual(PE)
  structure CartDict = Dict(CartEqual)
  structure CartSet = DictSet(CartDict)

  open CartSet

end
