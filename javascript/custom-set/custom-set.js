/* eslint no-underscore-dangle: ["error", { "allowAfterThis": true }] */
/* eslint-disable  space-in-parens */

/* so, yeah, kind of cheating to use a Set as the storage class */

export class CustomSet {
  constructor(elements) {
    this._elements = new Set(elements);
  }

  // Note, Set.prototype.values() returns an *iterator*.
  // Converted that into an array.
  get elements() {
    return [...this._elements.values()];
  }

  get size() {
    return this._elements.size;
  }

  empty() {
    return this.size === 0;
  }

  add(element) {
    this._elements.add(element);
    return this;
  }

  contains(element) {
    return this._elements.has(element);
  }

  // true if all my elements are contained in the other set
  subset(other) {
    return this.elements.every(elem => other.contains(elem));
  }

  // true if two sets share no elements
  disjoint(other) {
    return this.elements.every(elem => !other.contains(elem));
  }

  eql(other) {
    return this.size === other.size && this.subset(other);
  }

  // elements contained in both sets
  intersection(other) {
    return new CustomSet( this.elements.filter(e => other.contains(e)) );
  }

  // which of my elements are _not_ in other
  difference(other) {
    return new CustomSet( this.elements.filter(e => !other.contains(e)) );
  }

  // all the elements.
  union(other) {
    return new CustomSet( this.elements.concat( other.difference(this).elements ));
  }
}
