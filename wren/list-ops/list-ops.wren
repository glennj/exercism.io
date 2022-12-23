class ListOps {
  construct new() {
    _data = []
  }
  construct new(elements) {
    _data = elements[0..-1]
  }

  toList { _data[0..-1] }

  add(item) { _data.add(item) }

  iterate(iterator) {
    if (iterator == null) {
      if (_data.count == 0) return false
      return 0
    }
    if (iterator + 1 == _data.count) return false
    return iterator + 1
  }
  iteratorValue(iterator) { _data[iterator] }

  each(loopFn) {
    for (item in this) {
      loopFn.call(item)
    }
  }

  addAll(other) {
    other.each {|item| add(item) }
    return this   // allow addAll to be chained
  }

  +(other) { type.new().addAll(this).addAll(other) }

  // given a series of lists, combine all items in all lists into one flattened list
  static concat(lists) {
    var list = new()
    lists.each {|l| list.addAll(l)}
    return list
  }

  where(predicateFn) {
    var list = type.new()
    each {|item|
      if (predicateFn.call(item)) {
        list.add(item)
      }
    }
    return list
  }

  count {
    var count = 0
    each {|item| count = count + 1 }
    return count
  }

  map(transformFn) {
    var list = type.new()
    each {|item| list.add(transformFn.call(item)) }
    return list
  }

  reverse() {
    var items = []
    each {|item| items.insert(0, item) }
    return type.new(items)
  }

  // given an initial accumulator and a function, fold (reduce) each item
  // into the accumulator from the left using `function(accumulator, item)`
  reduce(initialValue, reducerFn) {
    var accum = initialValue
    each {|item| accum = reducerFn.call(accum, item) }
    return accum
  }

  // given an initial accumulator and a function, fold (reduce) each item
  // into the accumulator from the right using `function(item, accumulator)`
  reduceRight(initialValue, reducerFn) {
    var accum = initialValue
    reverse().each {|item| accum = reducerFn.call(item, accum) }
    return accum
  }
}
