(* For your convenience, the data format and types are listed again below! *)
(* POKEMON(name, type1, type2, HP, ATT, DEF, SPA, SPD, move_id_list) *)
(* Types: 0 - normal
          1 - fighting
          2 - flying
          3 - poison
          4 - ground
          5 - rock
          6 - bug
          7 - ghost
          8 - steel
          9 - fire
          10 - water
          11 - grass
          12 - electric
          13 - psychic
          14 - ice
          15 - dragon
          16 - dark
          17 - fairy
  Source: http://bulbapedia.bulbagarden.net/wiki/Type#Type_chart
*)


structure StandardOptions : CONSTS =
struct

val maxie_str = "Castform|0|~1|105|113|103|119|97|159|125|529|61*Celebi|13|11|108|141|120|148|104|159|559|537|225*Drapion|3|16|108|125|127|67|117|28|169|293|416*Mankey|1|~1|54|119|42|35|46|369|513|425|7*Pignite|9|1|140|102|57|80|83|230|314|10|568*Tyrantrum|5|15|113|169|141|80|75|288|490|524|220"

val minnie_str = "Thundurus|12|2|95|158|95|137|99|254|457|343|142*Amaura|5|14|122|84|81|99|88|216|14|590|238*Lotad|10|11|73|67|42|60|86|91|467|304|383*Ambipom|0|~1|98|121|71|104|75|77|449|394|377*Typhlosion|9|~1|126|129|119|115|88|431|153|139|34*Mewtwo|13|~1|149|130|105|166|135|4|545|584|60"

val maxie_data = GameData.string_to_data(maxie_str)
val minnie_data = GameData.string_to_data(minnie_str)

val level = 50
val fatigue_damage = 1
end

structure LowerLevelOptions : CONSTS =
struct

val maxie_data = [
    GameData.POKEMON("Pikachu",SOME(12),NONE,150,50,70,120,70,[21,85,86,446])
]
val minnie_data = [
    GameData.POKEMON("Eevee",SOME(0),NONE,120,80,60,50,70,[1,29,39,120])
]

val level = 50
val fatigue_damage = 1

end
