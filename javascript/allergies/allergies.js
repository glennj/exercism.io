/* eslint-disable  no-bitwise */

const Allergens = [
  { mask: 0b00000001, allergen: 'eggs' },
  { mask: 0b00000010, allergen: 'peanuts' },
  { mask: 0b00000100, allergen: 'shellfish' },
  { mask: 0b00001000, allergen: 'strawberries' },
  { mask: 0b00010000, allergen: 'tomatoes' },
  { mask: 0b00100000, allergen: 'chocolate' },
  { mask: 0b01000000, allergen: 'pollen' },
  { mask: 0b10000000, allergen: 'cats' },
];


export class Allergies {
  constructor(score) {
    this.allergens = Allergens.filter(({ mask }) => (score & mask)).map(e => e.allergen);
  }

  list() { return this.allergens; }

  allergicTo(thing) { return this.allergens.includes(thing); }
}
