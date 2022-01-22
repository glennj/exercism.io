// This class will have horrible performance!

export class List {
  constructor(elements) {
    this.values = [...(elements || [])];
  }

  length() {
    let i = 0;
    while (this.values[i] !== undefined) i += 1;
    return i;
  }

  // `append` modifies `this`
  append(other) {
    let i = this.length();
    let j = 0;
    while (other.values[j] !== undefined) {
      this.values[i] = other.values[j];
      i += 1;
      j += 1;
    }
    return this;
  }

  // `concat` does not modify `this`
  concat(listOfLists) {
    return listOfLists.foldl(
      (result, aList) => result.append(aList),
      new List().append(this)
    );
  }

  push(element) { this.values[this.length()] = element; }

  filter(func) {
    const result = new List();
    const len = this.length();
    for (let i = 0; i < len; i += 1) {
      if (func(this.values[i])) {
        result.push(this.values[i]);
      }
    }
    return result;
  }

  map(func) {
    const result = new List();
    const len = this.length();
    for (let i = 0; i < len; i += 1) {
      result.push(func(this.values[i]));
    }
    return result;
  }

  foldl(func, seed) {
    let accumulator = seed;
    const len = this.length();
    for (let i = 0; i < len; i += 1) {
      accumulator = func(accumulator, this.values[i]);
    }
    return accumulator;
  }

  reverse() {
    const result = new List();
    const len = this.length();
    for (let i = len - 1; i >= 0; i -= 1) {
      result.push(this.values[i]);
    }
    return result;
  }

  foldr(func, seed) {
    return this.reverse().foldl(func, seed);
  }
}
