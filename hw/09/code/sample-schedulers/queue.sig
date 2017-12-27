signature QUEUE =
   sig
       type t
       type key

       val compare : t * t -> order

       (* REQUIRES: true *)
       (* ENSURES: empty is a queue with no elements *)
       val empty : t

       (* REQUIRES: true *)
       (* ENSURES: insert k t returns a queue containing all
        * of the elements of t as well as k
        *)
       val insert : key -> t -> t

       (* REQUIRES: true *)
       (* ENSURES: pop t = SOME (k, t') where k is the first element
        * inserted into t and t' is t without k. If t is empty then
        * this returns NONE
        *)
       val pop : t -> (key * t) option

       (* REQUIRES: true *)
       (* ENSURES: reverse q = q' where q and q' contains
        * the same elements but in the opposite order*)
       val reverse : t -> t

       (* REQUIRES: true *)
       (* ENSURSES: map f q = q' where q''s ith element is the f e where e
        * is the ith element of q.
        *)
       val map : (key -> key) -> t -> t
   end
