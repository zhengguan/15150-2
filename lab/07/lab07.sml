(* 15-150 Lab 08 *)

exception Unimplemented
exception NotFound

datatype 'a tree = Empty
                 | Node of ('a tree) * 'a * ('a tree)


(* Datatype of Regular Expressions extended to include Wildcard characters *)
datatype regexp = Zero
                | One
                | Char of char
                | Wild
                | Plus of regexp * regexp
                | Times of regexp * regexp
                | Star of regexp

(* Task 2.1 : change the value here *)
val rml : regexp = Times(Star(Wild), Times(Char #".",
                   Times( Char #"s", Times (Char #"m", Char #"l" ))))

(* Task 2.2 : change the value here *)
val rname : regexp = Times(Wild,Times(Char #"l", Char #"i"))

(* match : regexp -> char list -> (char list -> bool) -> bool
   REQUIRES: a valid regexp, and p total
   ENSURES: (match r L p) returns true iff L=L1@L2 with L1 matching r
                                           exactly and p(L2) returning true.
*)
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
(* implement the wild case for match *)
      | Wild  => (case cs of
                       [] => false
                      |c' :: cs' => k cs')

val true = match rml [#"h",#"i",#".",#"s",#"m",#"l"] (fn L : char list => true)
val false = match rml [#"h",#"i",#".",#"s",#"m"] (fn L : char list => true)
val true = match Wild [#"h"] (fn L : char list => (L = []))

(* Task 2.4 : implement this function *)
(* accept : regexp -> string -> bool
 * REQUIRES: r be a valid regexp
 * ENSURES: accept returns true iff the regexp r matches the string s exactly *)
fun accept (r : regexp) (s : string) : bool =
    let
      val cList = String.explode(s)
    in
      match r cList (fn L : char list => true)
    end

val true = accept rml "hi.sml"
val false = accept rml "hi.sm"
val true = accept Wild "e"
val false = accept Wild ""
val true = accept rname "hli"

(* Task 2.5 : write at least two tests for match Wild. *)

(* Task 2.6 : implement this function *)
(* HINT: use List.filter as well as regular expressions or functions you have
 *   written previously in this section. *)
fun grep_ml (cs : string list) : string list =
    case cs of
      [] => []
     |x::xs => case (match rml (String.explode(x)) (fn L => (L = []))) of
                 true => x::(grep_ml xs)
                |false => grep_ml xs

val ["hi.sml"] = grep_ml ["hi.", "hi.sm", "hi.sml"]

(* Task 2.7 : implement this function  *)
fun nub (r : regexp) : regexp =
  raise Unimplemented

(* Task 3 : implement this function *)
fun zeroout (r : regexp) : regexp = raise Unimplemented

(* Task 4.1 : implement this function *)
fun embed (() : unit) : (('a -> exn) * (exn -> 'a option)) = raise Unimplemented

(* an alist is a map from strings ("names" of contents)
 * to values of arbitrary type.
 * The invariant maintained by the user is that the exn -> unit function
 * of the alist is capable of printing
 * any value the alist contains.
 *)
type alist = ((string * exn) list) * (exn -> string)

(* Task 4.2 : implement this function *)
fun add_type ((al, al_show_fun) : alist) (new_show_fun : 'a -> string)
      : (alist * ('a -> exn) * (exn -> 'a option)) = raise Unimplemented

(* Task 4.3 : implement this function *)
fun print_alist ((al, al_show_fun) : alist) : unit = raise Unimplemented

(* Task 4.4 : implement this function *)
fun add_to_alist ((al, f) : alist) (e : string * exn) : alist =
    raise Unimplemented
