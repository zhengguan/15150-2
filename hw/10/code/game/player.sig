signature PLAYER =
sig 
  structure Game : ESTGAME

  val next_move : Game.state -> Game.move
end
