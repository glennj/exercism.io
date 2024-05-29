import 'dart:math';

int d6() {
  return Random().nextInt(6) + 1;
}

class DndCharacter {
  static int modifier(int score) {
    return ((score - 10) / 2).floor();
  }

  static int ability() {
    var sum = 0;
    var min = 6;
    for (var i = 1; i <= 4; i++) {
      var die = d6();
      sum += die;
      min = die < min ? die : min;
    }
    return sum - min;
  }

  final int strength;
  final int dexterity;
  final int constitution;
  final int intelligence;
  final int wisdom;
  final int charisma;
  final int hitpoints;

  const DndCharacter(this.strength, this.dexterity, this.constitution,
      this.intelligence, this.wisdom, this.charisma, this.hitpoints);

  static DndCharacter create() {
    var strength = ability(),
        dexterity = ability(),
        constitution = ability(),
        intelligence = ability(),
        wisdom = ability(),
        charisma = ability();
    var hitpoints = 10 + modifier(constitution);

    return DndCharacter(strength, dexterity, constitution, intelligence, wisdom,
        charisma, hitpoints);
  }
}
