class Strain {
  static keep(list, predicate) {
    // trying to implement this without using any builtin higher-order functions.
    var result = []
    for (elem in list) {
      if (predicate.call(elem)) result.add(elem)
    }
    return result
  }

  static discard(list, predicate) { keep(list) {|elem| !predicate.call(elem) } }
}
