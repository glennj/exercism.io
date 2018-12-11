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

module.exports = Squares;

/* or ES6

const range = (a, b) => {
  const [m, n] = [a, b].sort((x, y) => x - y);
  return new Array(n - m + 1).fill(null).map((_, i) => m + i);
};
const upto = n => range(1, n);
const sum = list => list.reduce((s, n) => s + n, 0);

export default class Squares {
  constructor(n) {
    this.squareOfSum = sum(upto(n)) ** 2;
    this.sumOfSquares = sum(upto(n).map(a => a ** 2));
    this.difference = this.squareOfSum - this.sumOfSquares;
  }
}
*/
