/* eslint-disable  space-in-parens, no-restricted-syntax */

/* Example usages:
 *
 *     for (const even of from(2).upTo(100).by(2)) {...}
 *
 *     from(100).downTo(1).forEach(n => fn(n));
 *
 * Throws an error if you try to iterate before calling `downTo` or `upTo`
 */

class IterableRange {
  constructor(n) {
    this.current = n;
    this.step = 1;
  }

  downTo(n) {
    this.stop = n;
    this.direction = -1;
    return this;
  }

  upTo(n) {
    this.stop = n;
    this.direction = +1;
    return this;
  }

  by(n) {
    this.step = n;
    return this;
  }

  postinc() {
    const c = this.current;
    this.current += this.direction * this.step;
    return c;
  }

  [Symbol.iterator]() {
    return {
      next: () => {
        if (this.direction === undefined) {
          throw new Error('Don\'t know when to stop. Call `downTo(n)` or `upTo(n)` first.');
        }
        if ( (this.direction === +1 && this.current <= this.stop)
          || (this.direction === -1 && this.current >= this.stop)
        ) {
          return { value: this.postinc(), done: false };
        }
        return { done: true };
      },
    };
  }

  forEach(callback) {
    for (const i of this) {
      callback(i);
    }
  }

  map(callback) {
    const result = [];
    for (const i of this) {
      result.push(callback(i));
    }
    return result;
  }

  flatMap(callback) {
    const result = [];
    for (const i of this) {
      result.push(...callback(i));
    }
    return result;
  }

  reduce(callback, initialValue) {
    let result = initialValue;
    for (const i of this) {
      result = callback(result, i);
    }
    return result;
  }
}

export const from = (n) => {
  return new IterableRange(n);
};
