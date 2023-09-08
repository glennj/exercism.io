import std/math
import std/random
import sugar

type
  Character* = object
    strength*: int
    dexterity*: int
    constitution*: int
    intelligence*: int
    wisdom*: int
    charisma*: int
    hitpoints*: int

proc ability*: int =
  var d6 = () => rand(1..6)
  var roll = [d6(), d6(), d6(), d6()]
  sum(roll) - min(roll)

proc modifier*(n: int): int =
  floorDiv(n - 10, 2)

proc initCharacter*: Character =
  var constitution = ability()
  Character(
    strength:     ability(),
    dexterity:    ability(),
    intelligence: ability(),
    wisdom:       ability(),
    charisma:     ability(),
    constitution: constitution,
    hitpoints:    10 + modifier(constitution)
  )
