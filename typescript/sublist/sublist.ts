class List {
  private readonly values: Array<{}>
  constructor(...values: Array<{}>) { this.values = values }

  toString(): string {
    // \x1f is the ascii "field separator" character
    return this.values.join('\x1f')
  }

  compare(other: List): string {
    const a = this.toString()
    const b = other.toString()

    if (a === b) { return 'equal' }
    if (a.indexOf(b) !== -1) { return 'superlist' }
    if (b.indexOf(a) !== -1) { return 'sublist' }
    return 'unequal'
  }
}

export default List
