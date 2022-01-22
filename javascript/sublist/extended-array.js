import {from} from './iterable-range';

export class ExtendedArray extends Array {
  constructor(...args) {
    super(...args);

    // Add an instance generator function.
    // Inspired by https://ruby-doc.org/core-2.5.3/Enumerable.html#method-i-each_cons
    // I haven't figured out how to do this with regular instance method notation.
    this.eachConsecutive = function* (n) {
      for (const i of from(0).upTo(this.length - n)) {
        yield ExtendedArray.from(this.slice(i, i + n));
      }
    };
  }

  equals(other) {
    return this.every((elem, i) => elem === other[i]);
  }

  containsArray(other) {
    for (const sublist of this.eachConsecutive(other.length))
      if (sublist.equals(other))
        return true;
    return false;
  }

  toArray() {
    return Array.from(this);
  }

  product() {
    return this.reduce((p, n) => p * n, 1);
  }
}
