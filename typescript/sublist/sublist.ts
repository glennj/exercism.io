export class List {
  readonly values: number[]

  constructor(...values: number[]) {
    this.values = values
  }

  /* This method is not safe: it joins the lists to strings
   * using a character that's _unlikely_ to appear in data.
   * Unlikely but not impossible.
   */
  /*
  compare(other: List): string {
    const a = this.toString()
    const b = other.toString()

    if (a === b) { return 'equal' }
    if (a.indexOf(b) !== -1) { return 'superlist' }
    if (b.indexOf(a) !== -1) { return 'sublist' }
    return 'unequal'
  }

  toString(): string {
    // \x1f is the ascii "field separator" character
    return this.values.join('\x1f')
  }
  */

  get length(): number {
    return this.values.length
  }

  compare(other: List): string {
    if (this.equals(other))   return 'equal';
    if (this.contains(other)) return 'superlist';
    if (other.contains(this)) return 'sublist';
    return 'unequal';
  }

  equals(other: List) {
    if (this.length !== other.length) return false

    for (let i = 0; i < this.length; i++)
      if (this.values[i] !== other.values[i])
        return false

    return true
  }

  contains(other: List) {
    if (other.length > this.length) return false

    for (let i = 0; i <= this.length - other.length; i++)
      if (other.equals(new List(...this.values.slice(i, i + other.length))))
        return true

    return false
  }
}
