signature LABDICT =
sig

  (* We model a dictionary as a set of key-value pairs written k ~ v:
     (k1 ~ v1, k2 ~ v2, ...) *)
  type ('k, 'v) dict

  (* the empty mapping *)
  val empty : ('k, 'v) dict

  (* insert cmp (k1 ~ v1, ..., kn ~ vn) (k,v)
        == (k1 ~ v1, ..., ki ~ v,...) if cmp(k,ki) ==> EQUAL for some ki
     or == (k1 ~ v1, ..., kn ~ vn, k ~ v) otherwise
     *)
  val insert : ('k * 'k -> order) -> ('k, 'v) dict -> ('k * 'v) -> ('k, 'v) dict

  (* lookup cmp (k1 ~ v1,...,kn ~ vn) k == SOME vi
                                           if cmp(k,ki) ==> EQUAL for some ki
                                        == NONE otherwise
     *)
  val lookup : ('k * 'k -> order) -> ('k, 'v) dict -> 'k -> 'v option

  (*modify cmp d (key, value)  ==
        if (key, v) for some v exists in d then
                SOME d' where (key, v) not in d' and (key, value) in d'
        else NONE *)

  val modify : ('k * 'k -> order) -> ('k, 'v) dict -> ('k * 'v) -> ('k, 'v) dict option

end
