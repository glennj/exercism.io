class CustomSet<T> {
  // For the backend, instead of Set which feels cheat-y, use a Map, as they keys are unique.
  // The values will all be "undefined"
  private _elements: Map<T, undefined>

  constructor(elements: T[] = []) {
    // create an array of key-value pairs, so we can instantiate a Map
    const iterable: Array<[T, undefined]> = elements.reduce(
      // double brackets: concat flattens one level.
      (pairs, elem) => pairs.concat( [[ elem, undefined ]] ),
      new Array<[T, undefined]>()
    )
    this._elements = new Map(iterable)
  }

  get elements(): T[] {
    // Note, Set.prototype.values() returns an *iterator*.
    // Convert that into an array.
    return [...this._elements.keys()]
  }

  get size(): number {
    return this._elements.size
  }

  empty(): boolean {
    return this.size === 0
  }

  add(element: T): CustomSet<T> {
    this._elements.set(element, undefined)
    return this
  }

  contains(element: T): boolean {
    return this._elements.has(element)
  }

  // true if all my elements are contained in the other set
  subset(other: CustomSet<T>): boolean {
    return this.elements.every((elem) => other.contains(elem))
  }

  // true if two sets share no elements
  disjoint(other: CustomSet<T>): boolean {
    return this.elements.every((elem) => !other.contains(elem))
  }

  eql(other: CustomSet<T>): boolean {
    return this.size === other.size && this.subset(other)
  }

  // elements contained in both sets
  intersection(other: CustomSet<T>): CustomSet<T> {
    return new CustomSet( this.elements.filter((e) => other.contains(e)) )
  }

  // which of my elements are _not_ in other
  difference(other: CustomSet<T>): CustomSet<T> {
    return new CustomSet( this.elements.filter((e) => !other.contains(e)) )
  }

  // all the elements.
  union(other: CustomSet<T>): CustomSet<T> {
    return new CustomSet( this.elements.concat( other.difference(this).elements ))
  }
}

export default CustomSet
