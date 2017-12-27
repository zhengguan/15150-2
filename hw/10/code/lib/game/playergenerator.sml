(* Creates the players given the game. *)

functor MMTwo(G : ESTGAME) = MiniMax(struct structure G = G
                                            val search_depth = 2
                                     end)

functor MMFour(G : ESTGAME) = MiniMax(struct structure G = G
                                             val search_depth = 4
                                      end)

functor ABThree(G : ESTGAME) = AlphaBeta(struct structure G = G
                                                val search_depth = 3
                                         end)

functor ABTwo(G : ESTGAME) = AlphaBeta(struct structure G = G
                                              val search_depth = 2
                                       end)

functor ABFour(G : ESTGAME) = AlphaBeta(struct structure G = G
                                               val search_depth = 4
                                        end)

functor ABFive(G : ESTGAME) = AlphaBeta(struct structure G = G
                                               val search_depth = 5
                                        end)

functor ABSix(G : ESTGAME) = AlphaBeta(struct structure G = G
                                              val search_depth = 6
                                       end)
functor JamFour(G : ESTGAME) = Jamboree(struct structure G = G
                                           val search_depth = 4
                                           val prune_percentage = 0.5
                                    end)
functor JamSix(G : ESTGAME) = Jamboree(struct structure G = G
                                           val search_depth = 6
                                           val prune_percentage = 0.5
                                    end)
