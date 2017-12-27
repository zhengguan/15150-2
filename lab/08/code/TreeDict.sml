structure TreeDict : LABDICT=
struct

  (* UNCOMMENT AND DEFINE DATATYPE *)
  (* datatype ('a, 'b) tree = *)

  (* THIS IS JUST A PLACEHOLDER TYPE *)
  type ('a, 'b) dict = unit

  (* THIS IS JUST A PLACEHOLDER FOR EMPTY *)
  val empty = ()

  fun insert cmp d (k, v) = raise Fail "UI"

  fun lookup cmp d k = raise Fail "UI"

  fun modify cmp d (key,value) = raise Fail "UI"
end
