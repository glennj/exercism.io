// Courtesy: https://stackoverflow.com/a/20871714/7552
const permutator = (inputArr) => {
  let result = [];

  const permute = (arr, m = []) => {
    if (arr.length === 0) {
      result.push(m)
    } else {
      for (let i = 0; i < arr.length; i++) {
        let curr = arr.slice();
        let next = curr.splice(i, 1);
        permute(curr.slice(), m.concat(next))
     }
   }
 }

 permute(inputArr)

 return result;
};

/* ----------------------------------------------------------------
 * The clues, and the method that implements them:
 *   1. There are five houses.
 *   2. The Englishman lives in the red house. (solveForNationality)
 *   3. The Spaniard owns the dog. (solveForPets)
 *   4. Coffee is drunk in the green house. (solveForBeverages)
 *   5. The Ukrainian drinks tea. (solveForBeverages)
 *   6. The green house is immediately to the right of the ivory house.  (solveForColour)
 *   7. The Old Gold smoker owns snails. (solveForPets)
 *   8. Kools are smoked in the yellow house. (solveForSmokes)
 *   9. Milk is drunk in the middle house. (solveForBeverages)
 *  10. The Norwegian lives in the first house. (solveForNationality)
 *  11. The man who smokes Chesterfields lives in the house next to the man with the fox. (solveForPets)
 *  12. Kools are smoked in the house next to the house where the horse is kept.
 *  13. The Lucky Strike smoker drinks orange juice. (solveForSmokes)
 *  14. The Japanese smokes Parliaments. (solveForSmokes)
 *  15. The Norwegian lives next to the blue house. (solveForNationality)
 */

const PERMUTATIONS = permutator([1,2,3,4,5]);

const COLOURS = ['red', 'green', 'ivory', 'yellow', 'blue'];
const NATIONS = ['english', 'spanish', 'ukranian', 'norwegian', 'japanese'];
const BEVVIES = ['coffee', 'tea', 'milk', 'orangeJuice', 'water'];
const SMOKES  = ['oldGold', 'kools', 'chesterfields', 'luckyStrike', 'parliaments'];
const PETS    = ['dog', 'snails', 'fox', 'horse', 'zebra'];

const FIRST   = 1;
const MIDDLE  = 3;

const rightOf = (a, b) => a === b+1;
const nextTo = (a, b) => rightOf(a, b) || rightOf(b, a);

// --------------------------------------------------------------
class ZebraPuzzle {
  constructor() {
    this.drinkerOfWater = null;
    this.ownerOfZebra = null;
  }

  waterDrinker() {
    if (!this.drinkerOfWater) this.solve();
    return this.drinkerOfWater;
  }

  zebraOwner() {
    if (!this.ownerOfZebra) this.solve();
    return this.ownerOfZebra;
  }

  solve() {
    for (const p of PERMUTATIONS) {
      if (this.solveForColour(p)) break;
    }
    // not in the tests
    this.showNeighbourhood();
  }

  solveForColour(permutation) {
    COLOURS.forEach((colour, i) => this[colour] = permutation[i]);
    if (
      rightOf(this.green, this.ivory)             // clue 6
    ) {
      for (const p of PERMUTATIONS) {
        if (this.solveForNationality(p))
          return true;
      }
    }
  }

  solveForNationality(permutation) {
    NATIONS.forEach((nation, i) => this[nation] = permutation[i]);
    if (
        this.english === this.red                 // clue 2
        && this.norwegian === FIRST               // clue 10
        && nextTo(this.norwegian, this.blue)      // clue 15
    ) {
      this.nationalities = []
      this.nationalities[this.english] = "EnglishMan";
      this.nationalities[this.spanish] = "Spaniard";
      this.nationalities[this.ukranian] = "Ukranian";
      this.nationalities[this.norwegian] = "Norwegian";
      this.nationalities[this.japanese] = "Japanese";

      for (const p of PERMUTATIONS) {
        if (this.solveForBeverages(p))
          return true;
      }
    }
  }

  solveForBeverages(permutation) {
    BEVVIES.forEach((beverage, i) => this[beverage] = permutation[i]);
    if (
        this.coffee === this.green                // clue 4
        && this.ukranian === this.tea             // clue 5
        && this.milk === MIDDLE                   // clue 9
    ) {
      for (const p of PERMUTATIONS) {
        if (this.solveForSmokes(p))
          return true;
      }
    }
  }

  solveForSmokes(permutation) {
    SMOKES.forEach((smoke, i) => this[smoke] = permutation[i]);
    if (
        this.kools === this.yellow                // clue 8
        && this.luckyStrike === this.orangeJuice  // clue 13
        && this.japanese === this.parliaments     // clue 14
    ) {
      for (const p of PERMUTATIONS) {
        if (this.solveForPets(p))
          return true;
      }
    }
  }

  solveForPets(permutation) {
    PETS.forEach((pet, i) => this[pet] = permutation[i]);
    if (
        this.spanish === this.dog                 // clue 3
        && this.oldGold === this.snails           // clue 7
        && nextTo(this.chesterfields, this.fox)   // clue 11
        && nextTo(this.kools, this.horse)         // clue 12
    ) {
      this.drinkerOfWater = this.nationalities[this.water];
      this.ownerOfZebra = this.nationalities[this.zebra];
      return true;
    }
    return false;
  }

  // --------------------------------------------------------------
  showNeighbourhood() {
    const colours = this.sortAttributes(COLOURS);
    const nations = this.sortAttributes(NATIONS);
    const bevvies = this.sortAttributes(BEVVIES);
    const smokes  = this.sortAttributes(SMOKES);
    const pets    = this.sortAttributes(PETS);
    let result = [];
    colours.forEach((colour, i) => {
      result.push([i+1, colour, nations[i], bevvies[i], smokes[i], pets[i]].join());
    });
    console.log(result.join("\n"));
  }

  sortAttributes(varnames) {
    return varnames
      .map(name => [name, this[name]])
      .sort((a, b) => a[1] - b[1])
      .map(a => a[0]);
  }
}

module.exports = { ZebraPuzzle };
