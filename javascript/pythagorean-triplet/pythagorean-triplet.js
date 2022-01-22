class Triplet {
  constructor(a, b, c) {
    this.edges = [a, b, c];
  }

  sum() {
    return this.edges.reduce((sum, edge) => sum + edge);
  }

  isPythagorean() {
    const [a, b, c] = this.edges;
    return (c ** 2) === (a ** 2) + (b ** 2);
  }

  toArray() {
    return this.edges;
  }

  static where(conditions) {
    const { maxFactor, minFactor, sum } = conditions;
    const [min, max] = [minFactor ?? 1, maxFactor ?? Math.floor(sum / 2)];
    const result = [];
    for (let c = max; c >= min; c -= 1) {
      for (let b = c - 1; b >= min; b -= 1) {
        const a = Math.sqrt((c ** 2) - (b ** 2));
        if (Number.isInteger(a) && a > b) {
          const t = new Triplet(a, b, c);
          if (t.isPythagorean() && sum === t.sum()) {
            result.push(t);
          }
        }
      }
    }
    return result;
  }
}

export const triplets = (options) => Triplet.where(options);
