(* Transparent ascription, and implemented as lists, for debugging: *)

structure ListCore : SEQUENCECORE =
struct
  type 'a seq = 'a list

  exception Range of string

  val length = List.length
  val null = List.null
  fun nth n s = List.nth(s, n)
  fun tabulate f n = List.tabulate(n, f)
  val filter = List.filter
  val map = List.map
  val reduce = List.foldr

  datatype 'a lview = Nil | Cons of 'a * 'a seq
  datatype 'a tview = Empty | Leaf of 'a | Node of 'a seq * 'a seq

  fun showl nil = Nil
    | showl (x::xs) = Cons(x, xs)

  fun hidel Nil = nil
    | hidel (Cons (x,xs)) = x::xs

  fun showt s =
      case length s
       of 0 => Empty
        | 1 => Leaf (nth 0 s)
        | n =>
          let
            val mid = n div 2
          in
            Node (tabulate (fn i => nth i s) mid,
                  tabulate (fn x => nth (x + mid) s) (n - mid))
          end

  fun hidet Empty = nil
    | hidet (Leaf x) = [x]
    | hidet (Node(s1,s2)) = s1 @ s2

  fun toList s = s
  fun fromList L = L

  fun update (L, i, e) =
       if (i < 0) then raise Subscript
       else let
               fun up 0 (x::xs) = e::xs
                 | up i (x::xs) = x::(up (i-1) xs)
                 | up _ [] = raise Subscript
            in
               up i L
            end
end
