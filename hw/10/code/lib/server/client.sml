functor GClient (GCLData : CAPSULE) : GCLIENT =
struct

  (* reference to the socket connection to the server - NOT USED *)
  (* val socket : SocketUtil.socket option ref = ref(NONE) *)

  (* Some messages for constants *)
  val OK_MESSAGE = "OK"

  (* user data *)
  structure TestCLData = GCLData.ClientData

  (* all structures for riskless maps *)
  structure TestGame = GCLData.GameData (* current map *)

  (* edit this if you want to play yourself or via your estimator *)


  structure TestPlayer = AlphaBeta(struct
                                     structure G = TestGame
                                     val search_depth = 3
                                   end)
  (*
  structure TestPlayer = HumanPlayer(TestGame)
  *)

  (* maximum number of bytes to read *)
  val maxread : int = 1024

  fun connectToServer (host : string, port : int) : unit option =
      case SocketUtil.connectTo(host,port) of
        NONE => NONE
      | SOME(s) =>
        let
          (* Connected to Server *)
          val () = print "Successfully connected to server\n"
          val socket = s
          val SOME(motdmsg) = SocketUtil.read socket maxread NONE
          val () = print (motdmsg ^ "\n")
          (* val () = print "Read accept message from server...\n" *)
          (* Send Client Data and Wait for Server Response *)
          val () = print "\n * ===== .---. ===== .---------. ===== .---. ===== * "
          val () = print "\n * ===== | * | ===== |  GAME   | ===== | * | ===== * "
          val () = print "\n * ===== | * | ===== | SEARCH  | ===== | * | ===== * "
          val () = print "\n * ===== .---. ===== .---------. ===== .---. ===== * \n"
          val () = print "Waiting for a game...\n"
          (* Found a game, perform game pre-checks
           *  - SEND Username
           *  - RECV Usernames >> [Print Data...] >> SEND OK
           *  - RECV Game Code >> [Generate Game...] >> SEND OK
           *)
          fun raiseServerFail (msg : string) =
              raise Fail ("Invalid response from the server (post on Piazza) with message: " ^ msg)
          val SOME() = SocketUtil.write socket (TestCLData.username)
          val SOME(usernames) = SocketUtil.read socket maxread NONE
          val [maxie,minnie] = String.tokens (fn c => c = #" ") usernames
          val () = print Ansi.clearScreen
          val () = print ("* [BASIC TRAINER MATCH] >>>>> " ^ maxie ^ " VS. " ^ minnie ^ "\n")
          val SOME() = SocketUtil.write socket OK_MESSAGE
          val SOME(designation) = SocketUtil.read socket maxread NONE
          val () = print ("* [PLAYER DESIGNATION] >>>>> " ^ designation ^ "\n")
          val SOME() = SocketUtil.write socket OK_MESSAGE
          val SOME(config) = SocketUtil.read socket maxread NONE
          val ["SEND_GAME_GENERATOR", maxConfig, minConfig] = (String.tokens (fn c => c = #"$") config)
              handle _ => raiseServerFail "Unable to parse game generator"
          val start_state = TestGame.data_to_start_state [maxConfig, minConfig]
          val SOME() = SocketUtil.write socket OK_MESSAGE
          val () = print "* Game is now... STARTING!\n"
          (* Begin the Game! *)
          fun doTurn (currstate : TestGame.state, turnNumber : int) : int =
              let
                val () = if turnNumber > 0
                         then print ("\n --- [TURN " ^ (Int.toString turnNumber) ^ "] --- \n")
                         else ()
                val () = print ("\n" ^ TestGame.state_to_string currstate ^ "\n")
                val SOME(turnmsg) = SocketUtil.read socket maxread NONE
                val tokens = String.tokens (fn c => c = #"$") turnmsg
              in
                case tokens of
                  [keyword] => (case keyword of
                                  "SEND_MOVE" =>
                                  let
                                    val nextmove = TestPlayer.next_move(currstate)
                                    val nextstate = TestGame.make_move(currstate,nextmove)
                                    val tosend = TestGame.move_to_descriptor(nextmove) (* to send to the server *)
                                    val () = print Ansi.clearScreen
                                    val () = print ("[Your Move]\n")
                                    val () = print (" -- Move Found: " ^ tosend ^ "\n")
                                    val () = print (" >> Sending Move To Server... <<\n")
                                    val SOME(()) = SocketUtil.write socket tosend
                                    val () = print (" >> Move Sent! Awaiting Opponent... <<\n")
                                  in
                                    doTurn(nextstate, turnNumber+1)
                                  end
                                | _ => raiseServerFail turnmsg)
                | [keyword,arg1] => (case keyword of
                                       "APPLY_MOVE" =>
                                       let
                                         val SOME(nextmove) = TestGame.parse_move currstate arg1
                                         val nextstate = TestGame.make_move(currstate, nextmove)
                                         val () = print Ansi.clearScreen
                                         val () = print ("[Opponent Move]\n")
                                         val () = print (" -- Opponent Move: " ^ arg1 ^ "\n")
                                         val SOME() = SocketUtil.write socket OK_MESSAGE
                                       in
                                         doTurn(nextstate, turnNumber+1)
                                       end
                                     | _ => raiseServerFail turnmsg)
                | [keyword,arg1,arg2] => (case keyword of
                                            "GAME_RESULT" =>
                                            (case arg1 of
                                               "WIN" => (print ("\n>>>>> [WIN as " ^ designation ^ "] You won! Congratulations! (Reason: " ^
                                                                arg2 ^ ") <<<<<\n");1)
                                             | "LOSE" => (print ("\n>>>>> [LOSE as " ^ designation ^ "] You lost! Maybe next time... (Reason: " ^
                                                                 arg2 ^ ") <<<<<\n");~1)
                                             | _ => raiseServerFail turnmsg)
                                          | _ => raiseServerFail turnmsg)
                | _ => raiseServerFail turnmsg
              end
          (* Do Game 1 *)
          val rd1stat = doTurn(start_state, 1)
          val () = print ("* [TRAINER MATCH OVER] >>>>> " ^ maxie ^ " VS. " ^ minnie ^ "\n")
          val () = print "\n===== Game Is Over - Terminating Connection To Server... Have A Nice Day! =====\n"
          val () = SocketUtil.close(socket)
        in
          SOME(())
        end

end
