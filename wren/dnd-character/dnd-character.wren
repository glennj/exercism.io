import "random" for Random

class Character {
  static rollAbility() {
    var rolls = (1..4).map {Die.new(6).roll()}.toList
    var sum = rolls.reduce {|sum, roll| sum + roll}
    var min = rolls.reduce {|min, roll| min.min(roll)}
    return sum - min
  }

  construct new() {
    _data = {}
    ["str","dex","con","int","wis","cha"].each {|characteristic|
      _data[characteristic] = type.rollAbility()
    }
    _data["hit"] = 10 + Util.abilityModifier(_data["con"])
  }

  strength     { _data["str"] }
  dexterity    { _data["dex"] }
  constitution { _data["con"] }
  intelligence { _data["int"] }
  wisdom       { _data["wis"] }
  charisma     { _data["cha"] }
  hitpoints    { _data["hit"] }
}

class Util {
  static abilityModifier(n) {
    if (n <  3) Fiber.abort("Ability scores must be at least 3")
    if (n > 18) Fiber.abort("Ability scores can be at most 18")
    return ((n - 10) / 2).floor
  }
}

class Die {
  construct new(faces) {
    _faces = faces
  }
  static classInit() { __rand = Random.new(1234) } // TODO: remove seed
  roll() { 1 + __rand.int(_faces) }
}
Die.classInit()
