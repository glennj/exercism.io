/*
// old school
function Squares(n) {
  function range(x, y) {
    const [s, t] = [x, y].sort((a, b) => a - b);
    return new Array(t - s + 1).fill(null).map((_, i) => s + i);
  }
  function upto(x) { return range(1, x); }
  function sum(list) { return list.reduce((s, x) => s + x, 0); }

  this.squareOfSum = sum(upto(n)) ** 2;
  this.sumOfSquares = sum(upto(n).map(a => a ** 2));
  this.difference = this.squareOfSum - this.sumOfSquares;
}

module.exports = {Squares};

// or ES6
*/

import {from} from './iterable-range';

export class Squares {
  constructor(n) {
    this.n = n;
  }

  get squareOfSum() {
    const sum = from(1).upTo(this.n)
                  .reduce((sum, i) => sum + i, 0);
    return sum**2;
  }

  get sumOfSquares() {
    return from(1).upTo(this.n)
            .reduce((sum, i) => sum + i**2, 0);
  }

  get difference() {
    return Math.abs(this.sumOfSquares - this.squareOfSum);
  }
}
