signature STACK =
sig
  type key
  type stack
  type t = stack

  val compare : t * t -> order

  (* create an empty stack *)
  val empty : stack

  (* check if the stack is empty *)
  val null : stack -> bool

  (* add an element to the top of the stack *)
  val insert : key -> stack -> stack

  (* look at first element *)
  val peek : stack -> key option

  (* remove first element *)
  val pop : stack -> (key * stack) option

  (* Alternate getting values from each stack *)
  val interleave : stack * stack -> stack

  (* Load the first stack ahead of the second *)
  val compose : stack * stack -> stack
end
