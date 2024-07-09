// It would be nice to extend the List class,
// but the manual warns us not to extend the builtin types:
//    https://wren.io/classes.html#inheritance

class Permutater {
  construct new(list) {
    _list = list
  }

  permutations {
    if (_permutations == null) {
      _permutations = permutations_(_list)
    }
    return _permutations
  }

  permutations_(list) {
    if (list.count == 0) {
      return []
    } else if (list.count == 1) {
      return [list]
    } else {
      return list.reduce([]) {|perms, elem|
        var rest = list.where {|e| e != elem}.toList
        for (perm in permutations_(rest)) {
          perms.add([elem] + perm)
        }
        return perms
      }
    }
  }
}
