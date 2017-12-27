functor DictSet (Dict : DICT) : SET =
struct
	exception NotInSet
	exception Unimplemented

	(* replace unit with the correct types here *)
	type elem = Dict.key
	type set = unit Dict.dict

	fun empty () : set = Dict.empty ()

	fun void (S : set) : bool = (Dict.size S = 0)

	fun find (e : elem) (S : set) : bool =
            case (Dict.find S e) of
              NONE => false
             |SOME _ => true

	fun insert (e : elem) (S : set) : set =
            Dict.insert (fn a : unit * b : unit => ()) (e, ()) S

	fun delete (e : elem) (S : set) : set =
            Dict.delete e S

	fun union (s1 : set) (s2 : set) : set =
            case (void s1, void s2) of
              (true,_) => s2
             |(_,true) => s1
             |(_,_) => Dict.merge (fn a : unit * b : unit => ()) (s1, s2)

	fun intersection (s1 : set) (s2 : set) : set =
            Dict.filterk (fn e : elem => ((find e s1) andalso (find e s2))) s1

	fun difference (s1 : set) (s2 : set) : set =
            Dict.filterk (fn e : elem => ((find e s1) andalso not (find e s2)) s1

	fun toList s = raise Unimplemented
	fun fromList L = raise Unimplemented

end
