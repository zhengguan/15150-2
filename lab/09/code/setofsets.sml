functor SetOfSets (S : SET) : SET = 
struct
  	(* Implement PowerSet here *)
	exception Unimplemented

  	structure SetEqual : EQUAL =
	struct
		(* write SetEqual here *)
		(* replace unit with the correct type *)
		type t = unit
		
		fun equal (A : t, B : t) : bool = raise Unimplemented

	end


end
