/* eslint no-plusplus: ["error", { "allowForLoopAfterthoughts": true }] */

export default class Series {
  constructor(str) {
    this.digits = str.match(/\d/g).map(d => Number.parseInt(d, 10));
  }

  slices(n) {
    if (n > this.digits.length) throw new Error('Slice size is too big.');
    const result = [];
    for (let i = 0; i <= this.digits.length - n; i++) {
      result.push(this.digits.slice(i, i + n));
    }
    return result;
  }
}

/* community
 *
 * a tidy solution

      class Series {
        constructor(input) {
          this.digits = input.split('').map((v) => parseInt(v));
        }

        slices(len = 1) {
          let size = this.digits.length - len + 1;
          if (size < 1) {
            throw new Error('Slice size is too big.');
          }
          return Array(size).fill(null).map(
            (_, index) => this.digits.slice(index, index + len)
          );
        }
      }

      module.exports = Series;


 *
 */
