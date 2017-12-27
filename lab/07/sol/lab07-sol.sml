datatype regexp = Zero
                | One
                | Char of char
                | Wild
                | Plus of regexp * regexp
                | Times of regexp * regexp
                | Star of regexp

(* Task 2.1 : change the value here *)
val rdot = Char #"."
val rs = Char #"s"
val rm = Char #"m"
val rl = Char #"l"
val any = Star(Wild)
val rml : regexp = Times(any, Times(rdot, Times(rs, Times(rm, rl))))

(* Task 2.2 : change the value here *)
val rT = Char #"T"
val rA = Char #"A"
val rname : regexp = Times (Times (rT, rA), rs)
val rID : regexp = Times (Wild, Times (Star Wild, rname))

(* match : regexp -> char list -> (char list -> bool) -> bool
 * REQUIRES: k is total
 * ENSURES: (match r cs k) returns true iff cs=p@s with p matching r
 *                                         exactly and k(s) returning true. *)
fun match (r : regexp) (cs : char list) (k : char list -> bool) : bool =
    case r of
        Zero => false
      | One => k cs
      | Char a => (case cs of
                       []  => false
                     | c' :: cs' => (a = c') andalso k cs')
      | Plus (r1,r2) => match r1 cs k orelse match r2 cs k
      | Times (r1,r2) => match r1 cs (fn cs' => match r2 cs' k)
      | Star r =>
            let fun matchrstar cs = k cs orelse match r cs matchrstar
            in
                matchrstar cs
            end

(* Task 2.3 : implement the wild case for match *)
      | Wild => case (cs) of
	          [] => false  (* Shouldn't accept the empty list *)
	        | _::cs' => k(cs') (* Match the first character and continue *)



(* Task 2.4 : write at least two tests for match Wild. *)
(* We only want to accept strings that match the regex exactly. In other words,
 * the list of leftover characters should be empty. The "null" function it uses
 * is equivalent to matchCont: we're just writing the matchCont out so its
 * behavior is clear. *)
val matchCont = (fn l => case l of [] => true |_ => false)
val false = match Wild [] matchCont (* Wild shouldn't accept empty *)
val true = match Wild ([#"a"]) matchCont (* Wild should accept any single char *)
val false = match Wild ([#"a", #"b", #"c"]) matchCont
(* On its own, (i.e. without any other regex), Wild shouldn't accept more than
   a single character *)

(* Task 2.5 *)
(* accept : regexp -> string -> bool
 * REQUIRES: r is a standard regexp
 * ENSURES: accept r s returns true iff s is in the language of r *)
fun accept (r : regexp) (s : string) : bool =
    match r (String.explode s) (fn [] => true | _ => false)

(* Task 2.6 : implement this function *)
(* grep : string list -> string list
 * REQUIRES: true
 * ENSURES: all strings in cs that end in ".sml" are in the returning lis and
 *   every string in the returning list ends in ".sml" *)
fun grep_ml (cs : string list) : string list =
    List.filter (accept rml) cs

(* Task 2.7 : implement this function *)
(* nub : regexp -> regexp
 * REQUIRES: true
 * ENSURES: r' is a regexp that matches to everything in r except for "" *)
fun nub (r : regexp) : regexp =
   case (r) of
     One => Zero
   | Plus(r1,r2) => Plus(nub(r1), nub(r2))
   | Times(r1,r2) => Plus(Times(nub(r1), r2), Times(r1,nub(r2)))
   | Star(r) => Times(nub(r),Star(nub(r)))
   (* Covers the Zero, Wild and Char cases, which already reject
    * the empty string. *)
   |_ => r

(* Task 4 *)
(* zeroout : regexp -> regexp
 * REQUIRES: true
 * ENSURES: r' is Zero if r accepts nothing, and otherwise is a zero-simple 
 * regexp which accepts the same language as r *)
fun zeroout (r : regexp) : regexp =
  case r of
      Zero => Zero
    | One => One
    | Char x => Char x
    | Wild => Wild
    | Plus (r1, r2) => let val z1 = zeroout r1
                           val z2 = zeroout r2
                       in
                           case (z1, z2) of
                               (Zero, _) => z2
                             | (_, Zero) => z1
                             | (_, _) => Plus(z1, z2)
                       end
    | Times (r1, r2) => let val z1 = zeroout r1
                            val z2 = zeroout r2
                        in if (Zero=z1) orelse (Zero=z2)
                           then Zero
                           else Times (z1, z2)
                        end
    | Star r' => let val z' = zeroout r'
                 in if (Zero=z')
                    then Zero
                    else Star z'
                 end

(* Task 4 *)
(* embed : unit -> ('a -> exn) * (exn -> 'a option)
 * REQUIRES: true
 * ENSURES: returns (inject, project) such that inject produces a value of
 * type exn and project takes a value produced by inject and returns the
 * contained value of type 'a *)
fun embed (() : unit) : (('a -> exn) * (exn -> 'a option)) = let
    exception E of 'a
    fun project (packet : exn) = (raise packet)
      handle E e => SOME e
           | _ => NONE
  in
    (E, project)
  end

(* another way to write it, just for fun *)
fun embed (() : unit) : (('a -> exn) * (exn -> 'a option)) = let
    exception E of 'a
    fun project (packet : exn) = case packet of
        E e => SOME e
      | _ => NONE
  in
    (E, project)
  end

(* an alist is a map from strings ("names" of contents)
 * to values of arbitrary type.
 * The invariant maintained by the user is that the exn -> unit function
 * of the alist is capable of printing
 * any value the alist contains.
 *)
type alist = ((string * exn) list) * (exn -> string)

(* add_type : alist -> ('a -> string) ->
 *   (alist * ('a -> exn) * (exn -> 'a option) *)
fun add_type ((al, al_show_fun) : alist) (our_show_fun : 'a -> string)
      : (alist * ('a -> exn) * (exn -> 'a option))
    = let
    val (inject, project) = embed ()
    fun al_show_fun' e = case project e of
        SOME e' => our_show_fun e'
      | NONE => al_show_fun e
  in
    ((al, al_show_fun'), inject, project)
  end

(* print_alist : alist -> unit *)
fun print_alist ((al, al_show_fun) : alist) : unit =
  (print "{ ";
  List.app (fn (name, v) => print ("(" ^ name ^ ", "
                                     ^ al_show_fun v ^ "), ")) al;
  print "}\n")

(* add_to_alist : alist -> (string * exn) -> alist *)
fun add_to_alist ((al, f) : alist) (e : string * exn) = (e :: al, f)


(* the empty alist. The print function just does something silly. *)
val empty = ([], fn _ => raise Fail "nooo")
val (al, inject, _) = add_type empty (fn x => x)
val (al', inject', _) = add_type al Int.toString
val al'' = add_to_alist al' ("15", inject "150")
val al''' = add_to_alist al'' ("150", inject' 15)
val () = print "stuff happening! "
val () = print_alist al'''
