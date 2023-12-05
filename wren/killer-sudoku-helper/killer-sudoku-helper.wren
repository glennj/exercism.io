/* A set of lists of ints.
 *
 * Map keys must be "value types", which lists are not.
 * Since the list is a list of single digits, we can just accumulate them to a number.
 */
class CageSet {
  construct new() {
    _map = {}
    _toKey = Fn.new {|aList| aList.reduce(0) {|n, d| n * 10 + d}}
  }
  add(aList) { _map[_toKey.call(aList)] = aList }
  toList { _map.values.toList }
  toString { toList.toString }
}

// ------------------------------------------------------------
class KillerSudokuHelper {
  static combinations(sum, size, exclude) {
    if (size == 1) {
      if ((1..9).contains(sum) && !exclude.contains(sum)) {
        return [[sum]]
      } else {
        return []
      }
    } else {
      var combos = CageSet.new()
      for (n in 1..9) {
        if (!exclude.contains(n)) {
          for (c in combinations(sum - n, size - 1, exclude + [n])) {
            combos.add((c + [n]).sort())
          }
        }
      }
      return combos.toList
    }
  }
}
