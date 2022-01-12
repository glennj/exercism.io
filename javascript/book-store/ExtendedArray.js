/* Create a class that inherits from Array.
 * This is preferable to monkeypatching methods into Array.prototype
 */

export class ExtendedArray extends Array {
  constructor(...elements) {
    // Gee, javascript, thanks for doing different things with one argument.
    if (elements.length === 1) {
      super(1);
      this.fill(elements[0]);
    }
    else {
      super(...elements);
    }
  }

  toArray() {
    return this.reduce((a, e) => a.concat([e]), []);
  }

  isEmpty() {
    return this.length === 0;
  }

  // In order of appearance, the distinct elements of the array.
  // Returns an Array.
  distinct() {
    return this.reduce(
      (seen, element) => {
        if (!seen.includes(element)) {
          seen.push(element);
        }
        return seen;
      },
      []
    );
  }

  // Remove the _first_ instance of an element in an array.
  removeFirst(element) {
    const idx = this.indexOf(element);
    if (idx !== -1) {
      this.splice(idx, 1);
    }
    return;
  }

  /*
   * example
   *   set things {b c d d a b c c d c a d d d}
   *   orderedByCount $things
   *   #=> d d d d d d c c c c b b a a
   *
   * Note how the b's come before the a's: b was seen in the input before a
   */
  orderedByCount() {
    // maps maintain insertion order
    let counts = new Map();
    this.forEach(elem => counts.set(elem, (counts.get(elem) ?? 0) + 1));

    let pairs = [];
    counts.forEach((count, elem) => pairs.push([elem, count]));

    // relies on stable sorting
    return pairs
      .sort((a, b) => b[1] - a[1])
      .reduce(
        (sorted, [elem, count]) => sorted.concat(Array(count).fill(elem)),
        new ExtendedArray()
      );
  }
}
