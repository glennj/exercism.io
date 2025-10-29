import { Card } from './card';

const Hands = Object.freeze({
  STRAIGHT_FLUSH: 0,
  FOUR_OF_A_KIND: 1,
  FULL_HOUSE: 2,
  FLUSH: 3,
  STRAIGHT: 4,
  THREE_OF_A_KIND: 5,
  TWO_PAIR: 6,
  ONE_PAIR: 7,
  HIGH_CARD: 8,
});


export class PokerHand {
  static compare(a, b) {
    // smaller type is better; bigger ranking is better
    return a.type - b.type || b.ranking - a.ranking;
  }

  constructor(str) {
    this.orig = str;
    // sorted from highest to lowest
    this.cards = str
        .split(' ')
        .map((s) => new Card(s))
        .toSorted(Card.compare);

    [this.type, this.ranking] = this._analyze();
  }

  toString() {
    return this.orig;
  }

  _analyze() {
    var isFlush = this._isFlush();
    var isStraight = this._isStraight();

    var ranks = this.cards.map((c) => c.rank);

    // magic number: there are 14 cards in a suit (A can be high or low)
    var ranking = (...rs) => rs.reduce((acc, r) => acc * 14 + r);

    if (isFlush && isStraight) {
      return [Hands.STRAIGHT_FLUSH, ranking(ranks[0])];
    }
    if (isFlush) {
      return [Hands.FLUSH, ranking(...ranks)];
    }
    if (isStraight) {
      return [Hands.STRAIGHT, ranking(ranks[0])];
    }

    if (ranks[0] === ranks[3]) {
      return [Hands.FOUR_OF_A_KIND, ranking(ranks[0], ranks[4])];
    }
    if (ranks[1] === ranks[4]) {
      return [Hands.FOUR_OF_A_KIND, ranking(ranks[1], ranks[0])];
    }

    if (ranks[0] === ranks[2] && ranks[3] === ranks[4]) {
      return [Hands.FULL_HOUSE, ranking(ranks[0], ranks[3])];
    }
    if (ranks[0] === ranks[1] && ranks[2] === ranks[4]) {
      return [Hands.FULL_HOUSE, ranking(ranks[2], ranks[0])];
    }

    if (ranks[0] === ranks[2]) {
      return [Hands.THREE_OF_A_KIND, ranking(ranks[0], ranks[3], ranks[4])];
    }
    if (ranks[1] === ranks[3]) {
      return [Hands.THREE_OF_A_KIND, ranking(ranks[1], ranks[0], ranks[4])];
    }
    if (ranks[2] === ranks[4]) {
      return [Hands.THREE_OF_A_KIND, ranking(ranks[2], ranks[0], ranks[1])];
    }

    if (ranks[0] === ranks[1] && ranks[2] === ranks[3]) {
      return [Hands.TWO_PAIR, ranking(ranks[0], ranks[2], ranks[4])];
    }
    if (ranks[0] === ranks[1] && ranks[3] === ranks[4]) {
      return [Hands.TWO_PAIR, ranking(ranks[0], ranks[3], ranks[2])];
    }
    if (ranks[1] === ranks[2] && ranks[3] === ranks[4]) {
      return [Hands.TWO_PAIR, ranking(ranks[1], ranks[3], ranks[0])];
    }

    for (var i = 1; i < 5; i++) {
      if (ranks[i - 1] === ranks[i]) {
        var others = ranks.filter((_, j) => j < i - 1 || j > i);
        return [Hands.ONE_PAIR, ranking(ranks[i], ...others)];
      }
    }

    return [Hands.HIGH_CARD, ranking(...ranks)];
  }

  _isFlush() {
    var suits = new Set(this.cards.map((c) => c.suit));
    return suits.size === 1;
  }

  _isStraight() {
    var ranks = this.cards.map((c) => c.rank);

    // check for a 5,4,3,2,A straight.
    if ([13, 4, 3, 2, 1].every((rank, i) => rank === ranks[i])) {
      this.cards[0].lowAce();
      return true;
    }

    return ranks.every((r, i) => i === 0 || ranks[i - 1] === r + 1);
  }
}
