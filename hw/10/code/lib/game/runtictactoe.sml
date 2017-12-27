(* Some simple options to get you started *)
structure TTTThree = TicTacToe(struct val board_size = 3 end)
structure TTTFour = TicTacToe(struct val board_size = 4 end)
structure TTTFive = TicTacToe(struct val board_size = 5 end)
structure TTTSix = TicTacToe(struct val board_size = 6 end)

structure TicTacToe_HvAB = Referee(struct
                                   structure Maxie = HumanPlayer(TTTFour)
                                   structure Minnie = ABFour(TTTFour)
                                   end)

structure TicTacToe_HvMM = Referee(struct
                                   structure Maxie = HumanPlayer(TTTFive)
                                   structure Minnie = MMFour(TTTFive)
                                   end)

structure TicTacToe_HvH = Referee(struct
                                  structure Maxie = HumanPlayer(TTTFive)
                                  structure Minnie = HumanPlayer(TTTFive)
                                  end)

structure TicTacToe_HvJ = Referee(struct
                                  structure Maxie  = HumanPlayer(TTTFour)
                                  structure Minnie = JamSix(TTTFour)
                                  end)

structure TicTacToe_M2vM4 = Referee(struct
                                    structure Maxie  = MMTwo(TTTFive)
                                    structure Minnie = MMFour(TTTFive)
                                    end)

structure TicTacToe_M4vAB = Referee(struct
                                    structure Maxie = MMFour(TTTFour)
                                    structure Minnie = ABSix(TTTFour)
                                    end)

structure TicTacToe_JvM4 = Referee(struct
                                    structure Maxie = JamFour(TTTFive)
                                    structure Minnie = MMFour(TTTFive)
                                    end)

structure TicTacToe_ABvAB = Referee(struct
                                    structure Maxie = ABThree(TTTFive)
                                    structure Minnie = ABThree(TTTFive)
                                    end)

structure TicTacToe_JvAB = Referee(struct
                                   structure Maxie = JamFour(TTTFive)
                                   structure Minnie = ABFour(TTTFive)
                                   end)

structure TicTacToe_JvJ = Referee(struct
                                  structure Maxie = JamSix(TTTFive)
                                  structure Minnie = JamSix(TTTFive)
                                  end)
