structure GCLData : CAPSULE =
struct

  structure ClientData : CLIENTDATA = CDTest
  structure GameData : ESTGAME = Pokemon(StandardOptions)

end
