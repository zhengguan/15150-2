signature LHEAP =
sig
    type t
    type key

    val compare : t * t -> order

    (* REQUIRES: true *)
    (* ENSURES: empty is a leftist heap with no elements *)
    val empty : t

    (* REQUIRES: true *)
    (* ENSURES: insert k t returns a leftist heap containing all
     * of the elements of t as well as k
     *)
    val insert : key -> t -> t

    (* REQUIRES: true *)
    (* ENSURES: pop t = SOME (k, t') where k is the smallest element
     * of t and t' is t without k. If t is empty then this returns
     * NONE
     *)
    val pop : t -> (key * t) option

    (* REQUIRES: true *)
    (* ENSURES: head t = SOME k where k is smallest element of t.
     * If t is empty then this returns NONE
     *)
    val head : t -> key option

    (* REQUIRES: true *)
    (* ENSURES: delete t = t' where t' contains all of the elements
     * of t except the smallest. If t has no elements, then this just
     * returns t.
     *)
    val delete : t -> t
end
