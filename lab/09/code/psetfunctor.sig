signature POWERSETFUNCTOR =
sig
  type elem
  type set
  type powerset

  val make_powerset : set -> powerset
  val make_powerfun : (elem -> elem) -> (set -> set)
end

