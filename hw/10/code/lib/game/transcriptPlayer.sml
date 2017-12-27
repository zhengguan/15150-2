functor TranscriptPlayer(Args: sig val fname : string
                                   structure Players : TWO_PLAYERS end) : TWO_PLAYERS =
struct

  datatype mode = Read of TextIO.instream | Write of TextIO.outstream
  val curMode = ref(Read (TextIO.openIn Args.fname)
                       handle IO => Write(TextIO.openOut Args.fname))


  functor Transcript(P: PLAYER
             where type Game.state = Args.Players.Maxie.Game.state
             where type Game.move = Args.Players.Maxie.Game.move)  : PLAYER =
  struct
     structure Game  = Args.Players.Maxie.Game

     fun dropLast (s : string) =
         String.substring(s, 0, String.size s - 1)

     fun next_move (state: Game.state) =
         case !curMode of
           Read(file) =>
             (case TextIO.inputLine file of
                SOME s => (case Game.parse_move state (dropLast s) of
                             SOME m => m
                           | NONE => raise Fail "Bad Transcript")
              | NONE => (curMode := Write(TextIO.openAppend Args.fname); next_move state))
         | Write(file) =>
           let
             val move = P.next_move state
             val () = TextIO.output(file, Game.move_to_string move ^ "\n")
             val () = TextIO.flushOut(file)
           in
             move
           end

  end

  structure Maxie = Transcript(Args.Players.Maxie)
  structure Minnie = Transcript(Args.Players.Minnie)
end
