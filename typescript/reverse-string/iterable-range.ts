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
  private current: number
  private step: number
  private stop!: number
  private direction!: number

  constructor(n: number) {
    this.current = n;
    this.step = 1;
  }

  downTo(n: number) {
    this.stop = n;
    this.direction = -1;
    return this;
  }

  upTo(n: number) {
    this.stop = n;
    this.direction = +1;
    return this;
  }

  by(n: number) {
    this.step = n;
    return this;
  }

  postinc() {
    const c = this.current;
    this.current += this.direction * this.step;
    return c;
  }

  /* the Iterable protocol: Allows:
   *    const myrange = from(10).upTo(100).by(5)
   *    for (const i of myrange) {...}
   *
   * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Iteration_protocols
   * https://www.typescriptlang.org/docs/handbook/iterators-and-generators.html
   */
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

  forEach(callback: (n: number) => void): void {
    for (const i of this) {
      callback(i!);   // non-{null,undefined}
    }
  }

  map<T>(callback: (n: number) => T): T[] {
    const result = [];
    for (const i of this) {
      result.push(callback(i!));
    }
    return result;
  }

  flatMap<T>(callback: (n: number) => Iterable<T>): T[] {
    const result = [];
    for (const i of this) {
      result.push(...callback(i!));
    }
    return result;
  }

  reduce<T>(callback: (acc: T, elem: number) => T, initialValue: T): T {
    let result = initialValue;
    for (const i of this) {
      result = callback(result, i!);
    }
    return result;
  }
}

export const from = (n: number) => {
  return new IterableRange(n);
};
