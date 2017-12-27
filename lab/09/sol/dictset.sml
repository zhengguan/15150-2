functor DictSet (Dict : DICT) : SET =
struct

  exception NotInSet

  type elem = Dict.key

  type set = unit Dict.dict

  (* empty : unit -> set
   * return the empty set *)
  val empty = Dict.empty

  (* void : set -> bool
   * REQUIRES : true
   * ENSURES : determines whether or not a set is empty *)
  fun void (S : set) : bool = Dict.size S = 0

  (* find : elem -> set -> bool
   * REQUIRES : true
   * ENSURES : determines whether or not an element is in the set *)
  fun find (e : elem) (S : set) : bool =
    case Dict.find S e of
      NONE => false
      | _ => true

  (* insert : elem -> set -> set
   * REQUIRES : true
   * ENSURES : inserts an element e into the set S*)
  fun insert (e : elem) (S : set) : set =
    Dict.insert (fn (x,y) => ()) (e,()) S


  (* deletes : elem -> set -> set
   * REQUIRES : true
   * ENSURES : deletes an element from the set *)
  fun delete (e : elem) (S : set) : set =
    Dict.delete e S

  (* union : set -> set -> set
   * REQUIRES : true
   * ensures : takes the union of the two sets (no duplicates) *)
  fun union (s1 : set) (s2 : set) : set =
    Dict.merge (fn (x,y) => ()) (s1, s2)

  (* intersection : set -> set -> set
   * REQUIRES : true
   * ENSURES : instersection S1 S2 evaluates to a set that contains all
   *   of the elements in S1 and also in S2 *)
  fun intersection (s1 : set) (s2 : set) : set =
    Dict.filterk (fn x => find x s1) s2

  (* difference : set -> set -> set
   * REQUIRES : true
   * ENSURES : difference s1 s2 evaluates to a set that contains the elements
   *   in s1 that are not in s2 *)
  fun difference (s1 : set) (s2 : set) : set =
    Dict.filterk (fn x => not (find x s2)) s1


  (* toList : set -> elem list
   * REQUIRES : true
   * ENSURES : creates a list containing all elements in the set s *)
  fun toList (s : set) : elem list =
    List.map (fn(x,y) => x) (Dict.toList s)

  (* fromList : elem list -> set
   * REQUIRES : true
   * ENSURES : evaluates to a set containing all of the elements in L *)
  fun fromList (L : elem list) : set =
    Dict.fromList (List.map (fn x => (x,())) L)

end
