import {from} from './iterable-range';

export class ExtendedArray extends Array {
  constructor() {
    super();

    // Add an instance generator function.
    // Inspired by https://ruby-doc.org/core-2.5.3/Enumerable.html#method-i-each_cons
    // I haven't figured out how to do this with regular instance method notation.
    this.eachConsecutive = function* (n) {
      for (const i of from(0).upTo(this.length - n)) {
        yield ExtendedArray.from(this.slice(i, i + n));
      }
    };
  }

  toArray() {
    return Array.from(this);
  }

  product() {
    return this.reduce((p, n) => p * n, 1);
  }
}
