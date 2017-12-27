functor Queue(Key : ORDERED) : QUEUE =
struct

  (* Invariant, length head >= length tail *)
  datatype t = Queue of { head : Key.t list
                        , tail : Key.t list }
  type key = Key.t

  fun toList (Queue {head, tail}) = head @ rev tail

  fun compare (l, r) = List.collate Key.compare (toList l, toList r)

  val empty = Queue {head = [], tail = []}

  fun insert x (Queue {head, tail}) =
      case Int.compare (length head, length tail) of
          GREATER => Queue {head = head, tail = x :: tail}
        | _ => Queue {head = head @ rev (x :: tail), tail = []}

  fun delete (Queue {head = [], tail}) =
      (* Here we know tail is nil by invariant *)
      Queue {head = [], tail = []}
    | delete (Queue {head = _ :: xs, tail}) =
      case Int.compare (length xs, length tail) of
          LESS => Queue {head = xs @ rev tail, tail = []}
        | _ => Queue {head = xs, tail = tail}

  fun head (Queue {head, tail}) =
      case head of
          x :: _ => SOME x
        | [] => NONE

  fun pop q =
      case head q of
          SOME x => SOME (x, delete q)
        | NONE => NONE

  fun reverse q = Queue {head = rev (toList q), tail = []}

  fun map f (Queue {head, tail}) =
      Queue { head = List.map f head
            , tail = List.map f tail}
end

structure QueueTests =
struct
  structure Q = Queue(struct type t = int; val compare = Int.compare end)

  val NONE = Q.pop Q.empty

  val s = Q.insert
              1
              (Q.insert
                   2
                   (Q.insert
                        789
                        (Q.insert
                             456
                             (Q.insert 123 Q.empty))))
  val SOME(123,s') = Q.pop s
  val SOME(456, s'') = Q.pop s'
  val SOME(789, s''') = Q.pop s''

  val SOME (124, _) = Q.pop (Q.map (fn x => x + 1) s)

  val SOME (1, _) = Q.pop (Q.reverse s)
end
