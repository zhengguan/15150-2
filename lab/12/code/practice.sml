structure Practice =
struct
  datatype tree = Empty
                | Node of tree * int * tree

  (* task 6.1 *)
  (* treeSum : tree -> int
   * REQUIRES:
   * ENSURES:
   *)
  fun treeSum T = raise Fail "unimplemented"
  
  (* task 6.2 *)
  (* map' : ('a -> 'b) -> 'a list -> ('b list -> 'c) -> 'c
   * REQUIRES:
   * ENSURES:
   *)
  fun map' f L k = raise Fail "unimplemented"

  fun map f L = raise Fail "unimplemented"

  (* task 6.3 *)
  (* numNode' : tree -> (int -> 'a) -> 'a
   * REQUIRES:
   * ENSURES:
   *)
  fun numNode' T k = raise Fail "unimplemented"

  fun numNode T = raise Fail "unimplemented"

  (* task 6.5 *)
  (* combinations : int list -> int -> int
   * REQUIRES:
   * ENSURES:
   *)
  fun combinations L total = raise Fail "unimplemented"

end
