export class Card {
  static compare(a, b) {
    // descending order
    return b.rank - a.rank;
  }

  constructor(str) {
    this.label = str;
    var digit = str.at(0);
    switch (digit) {
      case 'A': this.rank = 13; break;
      case 'K': this.rank = 12; break;
      case 'Q': this.rank = 11; break;
      case 'J': this.rank = 10; break;
      case '1': this.rank =  9; break; // this is a ten
      default: this.rank = parseInt(digit) - 1;
    }
    this.suit = str.at(-1);
  }

  toString() {
    return this.label;
  }

  lowAce() {
    if (this.rank === 13) this.rank = 0;
  }
}
