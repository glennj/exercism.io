export class List {
  public values: unknown[] = []
  private _length: number = 0

  static create(...elements: unknown[]): List {
    return new this(elements);
  }

  constructor(elements: unknown[] = []) {
    for (const element of elements) {

      this.values[ this._length++ ] = element
    }
  }

  length(): number {
    return this._length
  }

  append(other: List): List {
    for (let j = 0; j < other.length(); j++) {
      this.values[ this._length++ ] = other.values[j]
    }
    return this
  }

  concat(other: List): List {
    other.forEach((elem) => {
      if (elem instanceof List) {
        this.append(elem)
      }
      else {
        this.push(elem)
      }
    })
    return this
  }

  // add/remove an element from start/end of list
  push(element: unknown): List {
    this.values[ this._length++ ] = element
    return this
  }

  unshift(element: unknown): List {
    this.values = [element, ...this.values]
    this._length++
    return this
  }

  pop(): unknown {
    const value: unknown = this.values.splice(this._length - 1, 1)[0]
    this._length--
    return value
  }

  shift(): unknown {
    const value: unknown = this.values.splice(0, 1)[0]
    this._length--
    return value
  }

  // iterate over the elements
  forEach( callback: (element: unknown, index?: number) => void ): void {
    for (let i = 0; i < this._length; i++) {
      callback( this.values[i], i )
    }
  }

  // implement foldl using forEach
  foldl<U, V>(
      callback: (accumulator: U, element: V) => U,
      seed: U
  ): U {
    let accumulator = seed
    this.forEach((element) => {
      accumulator = callback(accumulator, element as V)
    })
    return accumulator
  }

  // all the rest can be implemented using foldl
  map<T>( callback: (element: T) => unknown ): List {
    return this.foldl(
      (result, element) => result.push( callback(element as T) ),
      List.create()
    )
  }

  filter<T>( callback: (element: T) => boolean ): List {
    return this.foldl(
      (result, element) => {
        if (callback(element as T)) { result.push(element) }
        return result
      },
      List.create()
    )
  }

  reverse(): List {
    return this.foldl(
      (result, element) => result.unshift(element),
      List.create()
    )
  }

  foldr<U, V>(
      callback: (accumulator: U, element: V) => U,
      seed: U
  ): U {
    return this.reverse().foldl(callback, seed)
  }

}
