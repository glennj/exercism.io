// code to brute-force the a + b + c = 1000 solution

/* eslint-disable */
function findWhereSumIs1000() {
  const tried = new Set();
  let a, b, c;
  outer: for (a = 499; a >= 1; a -= 1) {
    for (b = Math.max(500, 1000 - 2 * a); b >= 1; b -= 1) {
      if (tried.has([a,b].sort())) continue;
      tried.add([a,b].sort());
      c = 1000 - a - b;
      if (c <= a || c <= b) continue;
      if (c ** 2 != a ** 2 + b ** 2) continue;
      break outer;
    }
  }
  console.log([a, b, c]);
}
/* eslint-enable */

// on to the main event
/* eslint-disable  no-multi-spaces, prefer-const */

class Triplet {
  constructor(a, b, c) {
    if (c <= a || c <= b) throw new Error('the hypoteneuse is too short');
    this.Sum = a + b + c;
    this.Product = a * b * c;
    this.edges = [a, b, c];
  }

  sum() {
    return this.Sum;
  }

  product() {
    return this.Product;
  }

  isPythagorean() {
    const [a, b, c] = this.edges;
    return (c ** 2) === (a ** 2) + (b ** 2);
  }

  toString() {
    return `[${this.edges.join(',')}]`;
  }

  static where(conditions) {
    const { maxFactor, minFactor, sum } = conditions;
    const [min, max] = [minFactor || 1, maxFactor];
    const result = [];
    for (let c = max; c >= min; c -= 1) {
      for (let b = c - 1; b >= min; b -= 1) {
        const a = Math.sqrt((c ** 2) - (b ** 2));
        if (Number.isInteger(a) && a > b) {
          const t = new Triplet(a, b, c);
          if (t.isPythagorean() && (!sum || sum === t.sum())) {
            result.push(t);
          }
        }
      }
    }
    return result;
  }
}

module.exports = Triplet;
