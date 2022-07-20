/* zip two lists
 *
 *     > import "./zip" for Zip
 *     > var z = Zip.new([11,12,13], [21,22,23,24,25])
 *     > z.toList
 *     [[11, 21], [12, 22], [13, 23]]
 *     > for (pair in z) { System.print(pair) }
 *     [11, 21]
 *     [12, 22]
 *     [13, 23]
 *
 *  and because it inherits from Sequence
 *
 *     > z.reduce(0) {|sum, pair| sum + pair[0]*pair[1]}
 *     794
 *     > z.where {|pair| pair[0] > 11}.toList
 *     [[12, 22], [13, 23]]
 *
 */

class Zip is Sequence {
  static transpose(lists) { new(lists).toList }

  construct new(list1, list2) {
    new([list1, list2])
  }

  construct new(lists) {
    // TODO assert lists has at least count 2
    _lists = lists[0..-1]
    _count = minCount_
  }

  minCount_ {
    var min = _lists[0].count
    for (lst in _lists.skip(1)) {
      var c = lst.count
      min = min < c ? min : c
    }
    return min
  }

  count { _count }

  toList {
    var list = []
    for (pair in this) {
      list.add(pair)
    }
    return list
  }

  iterate(iter) {
    if (iter == null) {
      if (count == 0) {
        return false
      }
      return 0
    }
    iter = iter + 1
    if (iter == count) {
      return false
    }
    return iter
  }

  //iteratorValue(iter) { [_list1[iter], _list2[iter]] }
  iteratorValue(iter) { _lists.map {|lst| lst[iter]}.toList }
}
