const abilityModifier = (n) => {
  if (n < 3)  { throw new Error('Ability scores must be at least 3') }
  if (n > 18) { throw new Error('Ability scores can be at most 18') }
  return Math.floor((n - 10) / 2);
}

// a 6-sided die roll
const d6 = () => 1 + Math.floor(Math.random() * 6);

const characteristics = [
  'strength', 'dexterity', 'constitution',
  'intelligence' , 'wisdom', 'charisma'
];

class Character {
  constructor() {
    characteristics.forEach((c) => {
      this[c] = Character.rollAbility();
    });
    this.hitpoints = 10 + abilityModifier(this.constitution);
  }

  static rollAbility() {
    const rolls = [0,0,0,0].map(d6);
    rolls.sort();
    return rolls.slice(1).reduce((sum, r) => sum + r);
  }
}

module.exports = { Character, abilityModifier };
