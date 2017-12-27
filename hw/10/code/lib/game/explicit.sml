structure GameTree =
struct

    datatype player = Minnie | Maxie

    datatype tree =
        Esti of string * int
      | Node of string * tree list

end

(* useful for testing search algorithms that use the estimator;
   unbounded complete search would loop.
 *)
functor ExplicitGame (A: sig
                             val tree : GameTree.tree
                         end) : ESTGAME where type move = int =
struct

    open GameTree

    datatype outcome = Winner of player
    datatype status = Over of outcome | In_play

    structure Est = Estimate
    type est = Est.est

    type move = int

    datatype absstate = S of (tree * player * int)
    type state = absstate

    val move_eq = (op =)
    (* ignore this, you guys shouldn't be editing this file anyways *)
    val is_valid_move = fn(s,e) => true

    fun status (S t) = In_play

    fun moves (s as S (t, _, _)) =
        case t of
            Esti v => Seq.tabulate (fn x => x) 1
          | Node (_,succs) => Seq.tabulate (fn x => x) (List.length succs)

    fun player (S (_, p, _)) = p

    val data_to_start_state = fn L => S (A.tree, Maxie, 0)
    val start = S (A.tree, Maxie, 0)

    fun turn (S(_,_,t)) = t

    fun make_move (s as S (t,p,trn), i) =
        case t of
            Esti _ => raise Fail "called make_move on an Esti state"
          | Node (_,next) => S(List.nth (next,i), case p of Maxie => Minnie | Minnie => Maxie,trn+1)

    (* estimate the value of the state, which is assumed to be In_play *)
    fun estimate (S(t,p,_)) =
        case t of
            Esti(s,v) => (print ("Estimating state " ^ s ^ "[" ^ Int.toString v ^ "]\n") ; Est.Guess(v))
          | _ => raise Fail "called estimate on a non-estimate node"

    fun outcome_to_est (Winner Minnie) = Est.MinnieWins
      | outcome_to_est (Winner Maxie) = Est.MaxieWins

    fun outcome_to_string (Winner Maxie) = "Maxie wins!"
      | outcome_to_string (Winner Minnie) = "Minnie wins!"

    fun player_to_string Maxie = "Maxie"
      | player_to_string Minnie = "Minnie"

    val move_to_string = Int.toString
    val detailed_move_to_string = fn (_,m) => move_to_string(m)
    val move_to_descriptor = move_to_string

    fun state_to_string (S(t,p,_)) = (case p of Maxie => "(Maxie," | Minnie => "(Minnie,") ^
        (case t of
            Esti(s,_) => s
          | Node(s,_) => s) ^ ")"

    fun parse_move s = raise Fail ""

    val mapID : string = "EXPLICIT"

end

(* run with search depth 2*)
structure HandoutSmall : ESTGAME =
ExplicitGame(struct
             open GameTree
             val tree = Node ("a",
                              [Node("b",
                                    [Esti("c", 3),Esti("d",6),Esti("e",~2)]),
                               Node("f",
                                    [Esti("g", 6),Esti("h",4),Esti("i",10)]),
                               Node("j",
                                    [Esti("k", 1),Esti("l",30),Esti("m",9)])])
             end)

(* run with search depth 4*)
structure HandoutBig : ESTGAME =
ExplicitGame(struct
                 open GameTree
                 val tree = Node ("a",
                                  [Node("b",
                                        [Node("c",[Node("d",[Esti("e",3),Esti("f",5)]),
                                                   Node("g",[Esti("h",2),Esti("i",7)])]),
                                         Node("j",[Node("k",[Esti("l",10),Esti("m",4)])])]),
                                   Node("n",
                                        [Node("o",[Node("p",[Esti("q",2),Esti("r",7)])]),
                                         Node("s",[Node("t",[Esti("u",8),Esti("v",2)]),
                                                   Node("w",[Esti("x",4),Esti("y",6)])])])])
             end)

structure BrokenTree : ESTGAME =
ExplicitGame(struct
  open GameTree
  val tree = Node ("a", [Node("b", [Node("c", [Esti("d",4)])]),
    Node("e", [Node ("f", [Esti("g",5)]),
      Node ("h", [Esti("i",6)])])])
  end)
