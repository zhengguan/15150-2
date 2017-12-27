structure Tree =
struct
  datatype 'a tree = Empty
                   | Node of 'a tree * 'a * 'a tree
  
  datatype decision = Left | Right
  
  type elem = int
  
  type stree = elem tree
  
  type result = decision list
  
  fun branch Empty = []
    | branch (Node (l,x,r)) = [(Left,l),(Right,r)]
  
  fun atRoot (j,Node (_,x,_)) = (j = x)
    | atRoot _ = false

end
