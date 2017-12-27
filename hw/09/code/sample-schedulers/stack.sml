functor Stack (Key: ORDERED): STACK =
struct

   type key = Key.t
   datatype stack = S of key list
   type t = stack


   fun compare (S l1, S l2) = List.collate (Key.compare) (l1,l2)

   val empty = S []

   (* Removes the datatype constructor *)
   fun strip (S l ) = l

   val null = List.null o strip

   fun insert (x : key) (S l : stack) = S(x::l)

   fun peek (S []) = NONE
     | peek (S (x::_)) = SOME x

   fun pop (S []) = NONE
     | pop (S (x::xs)) = SOME(x,S xs)

   fun interleave (s1 : stack, s2 : stack): stack =
       case (s1,s2) of
         (S [] , _) => s2
       | (_, S []) => s1
       | (S (x::xs), S(y::ys)) => insert x (insert y (interleave (S xs,S ys)))

   fun compose (S(l1) : stack, S(l2) : stack) = S(l1 @ l2)

end

structure StackTests =
struct
  structure S = Stack(struct type t = int; val compare = Int.compare end)

  val NONE = S.pop S.empty

  val s = S.insert 789 (S.insert 456 (S.insert 123 S.empty))
  val SOME(789,s') = S.pop s
  val SOME(456, s'') = S.pop s'
  val SOME(123, s''') = S.pop s''
  val NONE = S.pop s'''
end
