/* Using a Map where the keys are distinct */

class CustomSet {
  construct new() {
    empty()
  }

  construct new(iterable) {
    empty()
    for (elem in iterable) {
      add(elem)
    }
  }

  // remove all elements
  empty() { _set = {} }

  // add the given element
  add(elem) { 
    _set[elem] = 1 
    return this
  }

  count { _set.count }

  isEmpty { _set.isEmpty }

  contains(elem) { _set.containsKey(elem) }

  // all of my elements are contained in the other set
  subset(other) {
    assertSet(other)
    return difference(other).isEmpty
  }

  // none of my elements are contained in the other set
  disjoint(other) {
    assertSet(other)
    return intersection(other).isEmpty
  }

  // same size and same elements
  eql(other) {
    assertSet(other)
    return (count == other.count && subset(other))
  }

  // new set contains all elements in me and other
  union(other) {
    assertSet(other)
    return this + other.difference(this)
  }

  // new set containing elements in common between me and other
  intersection(other) {
    assertSet(other)
    return type.new(_set.keys.where {|elem| other.contains(elem)})
  }

  // new set containing my elements that are not contained in other
  difference(other) {
    assertSet(other)
    return type.new(_set.keys.where {|elem| !other.contains(elem)})
  }

  /* some utility methods */

  assertSet(other) {
    if (other.type != type) {
      Fiber.abort("parameter is not a Set")
    }
  }

  toList { _set.keys.toList }

  +(other) {
    assertSet(other)
    return type.new(toList + other.toList)
  }

  // Implement the iterator protocol.
  // Allows: `for (elem in aSet) {...}`
  iterate(iter) { _set.iterate(iter) }
  iteratorValue(iter) { _set.iteratorValue(iter).key }
}
