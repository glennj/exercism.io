/* eslint-disable  class-methods-use-this, no-extend-native, no-restricted-syntax */
/* eslint-disable  func-names, space-before-function-paren */

import BigInt from './lib/big-integer';

// ref: https://exercism.io/tracks/javascript/exercises/beer-song/solutions/146b391174db4b49a1e559db9096273
import { from } from '../lib/my-range-iterable';

// monkey patching Map to add `reduce` functionality
Map.prototype.reduceValues = function(callback, seed) {
  let acc = seed;
  for (const value of this.values()) {
    acc = callback(acc, value);
  }
  return acc;
};


/* ********************************************************************* */
class Grains {
  constructor(squares = 8 * 8) {
    this.cache = new Map([[1, BigInt(1)]]);
    from(2).upTo(squares).forEach(i => this.cache.set(i, this.cache.get(i - 1).times(2)));
    this.sum = this.cache.reduceValues((s, v) => s.plus(v), BigInt(0));
  }

  square(n) {
    return this.cache.get(n).toString();
  }

  total() {
    return this.sum.toString();
  }
}

module.exports = Grains;
