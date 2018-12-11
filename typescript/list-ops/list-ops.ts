class List<T = number> {
  public values: T[] = []
  private _length: number = 0

  constructor(elements: T[] = []) {
    for (const element of elements) {
      this.values[ this._length++ ] = element
    }
  }

  length(): number {
    return this._length
  }

  // modifies this
  append(other: List<T>): List<T> {
    for (let j = 0; j < other.length(); j++) {
      this.values[ this._length++ ] = other.values[j]
    }
    return this
  }

  // does not modify this
  concat(other: List<T>): List<T> {
    return new List<T>().append(this).append(other)
  }

  // add/remove an element from start/end of list
  push(element: T): List<T> {
    this.values[ this._length++ ] = element
    return this
  }

  unshift(element: T): List<T> {
    this.values = [element, ...this.values]
    this._length++
    return this
  }

  pop(): T {
    const value: T = this.values.splice(this._length - 1, 1)[0]
    this._length--
    return value
  }

  shift(): T {
    const value: T = this.values.splice(0, 1)[0]
    this._length--
    return value
  }

  // iterate over the elements
  forEach( callback: (element: T, index?: number) => void ): void {
    for (let i = 0; i < this._length; i++) {
      callback( this.values[i], i )
    }
  }

  // implement foldl using forEach
  foldl<U>(
      callback: (accumulator: U, element: T) => U,
      seed: U
  ): U {
    let accumulator = seed
    this.forEach((element) => {
      accumulator = callback(accumulator, element)
    })
    return accumulator
  }

  // all the rest can be implemented using foldl
  map<U>( callback: (element: T) => U ): List<U> {
    return this.foldl(
      (result, element) => result.push( callback(element) ),
      new List<U>()
    )
  }

  filter( callback: (element: T) => boolean ): List<T> {
    return this.foldl(
      (result, element) => {
        if (callback(element)) { result.push(element) }
        return result
      },
      new List<T>()
    )
  }

  reverse(): List<T> {
    return this.foldl(
      (result, element) => result.unshift(element),
      new List<T>()
    )
  }

  foldr<U>(
      callback: (accumulator: U, element: T) => U,
      seed: U
  ): U {
    return this.reverse().foldl(callback, seed)
  }

}

export default List
