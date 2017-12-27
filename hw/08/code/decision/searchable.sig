signature SEARCHABLE =
sig
   type stree
   type elem
   type decision
   type result = decision list
   val branch : stree -> (decision * stree) list
   val atRoot : (elem * stree) -> bool
end
