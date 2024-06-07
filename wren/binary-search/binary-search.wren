/* An over-complicated solution: I'll define the Iterator protocol
 * for the class. Then the `find` static function will create an
 * instance and iterate over it to find the target.
 *
 * https://wren.io/control-flow.html#the-iterator-protocol
 */

class BinarySearch {
  static find(values, target) {
    // assumes the values are pre-sorted.
    var searcher = this.new(values)
    var idx = searcher.indexOf(target)

    if (idx == -1) Fiber.abort("value not in list")
    return idx
  }

  construct new(values) {
    _values = values
    _target = null
  }

  target=(target) { _target = target }

  indexOf(target) {
    this.target = target
    for (idx in this) {
      if (_values[idx] == _target) {
        return idx
      }
    }
    return -1
  }

  // The iterator state is a {left, right} map of indices
  // Signal the end of the iteration by returning false.
  iterate(iterator) {
    // initializing the iteration
    if (iterator == null) {
      return _values.isEmpty ? false : {"left": 0, "right": _values.count - 1}
    }

    // return the next {left, right} index map
    var mid = iteratorValue(iterator)
    if (_target > _values[mid]) {
      iterator["left"] = mid + 1
    } else {
      iterator["right"] = mid - 1
    }
    return (iterator["left"] > iterator["right"]) ? false : iterator
  }

  // The loop value is set to the midpoint of the left and right indices
  iteratorValue(iterator) { ((iterator["left"] + iterator["right"]) / 2).floor }
}
