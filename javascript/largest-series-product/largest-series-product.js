/* eslint-disable  no-extend-native, func-names, no-restricted-syntax */

// inspired by https://ruby-doc.org/core-2.5.3/Enumerable.html#method-i-each_cons
Array.prototype.eachConsecutive = function* (n) {
  for (let i = 0; i <= this.length - n; i += 1) {
    yield this.slice(i, i + n);
  }
};

const product = numbers => numbers.reduce((prod, n) => prod * n, 1);

class Series {
  constructor(input) {
    if (/\D/.test(input)) throw new Error('Invalid input.');
    this.digits = Array.from(input).map(n => parseInt(n, 10));
  }

  largestProduct(n) {
    if (n < 0) throw new Error('Invalid input.');
    if (n > this.digits.length) throw new Error('Slice size is too big.');
    const products = [];
    for (const tuple of this.digits.eachConsecutive(n)) {
      products.push(product(tuple));
    }
    return Math.max(...products);
  }
}

module.exports = Series;
