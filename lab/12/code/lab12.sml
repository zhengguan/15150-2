structure CyclicList =
struct
  datatype 'a cell = Nil
                   | Cons of 'a * 'a llist
  withtype 'a llist = ('a cell) ref

  (* Task 3.1 *)
  (* printLlist : (’a -> string) -> ’a llist -> unit
   * REQUIRES: toString is total
   * ENSURES: printLlist toString l prints a string representation l
   *)
  fun printLlist (toString : 'a -> string) (l : 'a llist) : unit =
      case (!l) of
        Nil => ()
       |Cons of (x, l') => ((print(toString(x));(printLlist toString l'))

  (* Task 3.3 *)
  (* llistEq : ’a llist * ’a llist -> bool
   * REQUIRES:
   * ENSURES:
   *)
  fun llistEq (l : 'a llist, l' : 'a llist) : bool =
      op=

  (* Task 3.4 *)
  (* isCyclic : ’a llist -> bool
   * REQUIRES:
   * ENSURES:
   *)
  fun isCyclic (l : 'a llist) : bool =
      let
         fun tort (l' : 'a llist) : 'a llist =
             case (!l') of
               Nil => l'
              |Cons(x, l'') => l''
         fun hare (l' : 'a llist) : 'a llist =
             tort(tort(l'))
         fun loop (l' : 'a llist) : bool =
             case (!(tort(l')),(!hare(l')) of
               (Nil,_) => false
              |(_,Nil) => false
              |_ => llistEq((tort l'),(hare l')) orelse loop (!(tort l'))
      in
        loop l
      end
end

structure TestCyclicList =
struct
  (* Test your functions comprehensively here *)
end

structure Regex =
struct
  datatype regex =
           Zero
         | One
         | Char of char
         | Plus of regex * regex
         | Times of regex * regex
         | Star of regex

  (* Task 4.1 *)
  (* zeroout: regex -> regex
   * REQUIRES:
   * ENSURES:
   *)
  fun zeroout (R: regex) : regex =
    case R of
      Zero => Zero
    | One => One
    | Char x => Char x
    | Plus(R1, R2) => raise Fail "unimplemented"
    | Times(R1, R2) => raise Fail "unimplemented"
    | Star(R1) => raise Fail "unimplemented"
end

structure TestRegex =
struct
  (* Test your functions comprehensively here *)
end
