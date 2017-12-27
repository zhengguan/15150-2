(* code for the proof task; you don't need to modify or submit this file *)

signature QUEUE=
sig
   type queue
   val emp : queue
   val ins : int * queue -> queue
   val rem : queue -> (int * queue) option
end

structure LQ : QUEUE =
struct
     type queue = int list
    
     infix @
     fun ([] : 'a list) @ (L2 : 'a list) : 'a list = L2
       | (y::ys) @ L2 = y::(ys @ L2)

     val emp = []

     fun ins (n,l) = l @ [n]

     fun rem [] = NONE
       | rem (y::ys) = SOME (y, ys)
end

structure LLQ : QUEUE =
struct
     type queue = (int list) * (int list)
     
     infix @
     fun ([] : 'a list) @ (L2 : 'a list) : 'a list = L2
       | (y::ys) @ L2 = y::(ys @ L2)

     fun rev ([] : 'a list) : 'a list = []
       | rev (y::ys) = (rev ys) @ [y]

     val emp = ([], [])

     fun ins (n, (front, back)) = (front, n::back)

     fun rem (front, back) =
       case (front, back) of
         ([],[]) => NONE
       | (y::ys,_) => SOME (y,(ys,back))
       | ([],_) => rem (rev back,[])
end
