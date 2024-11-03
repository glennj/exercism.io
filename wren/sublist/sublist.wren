class Sublist {
  static sublist(listOne, listTwo) {
    if (listOne.count == listTwo.count) {
      if (listsEqual(listOne, listTwo)) {
        return "equal"
      }
    } else {
      // a will be the smaller list
      var a = listOne
      var b = listTwo
      var result = "sublist"

      if (listOne.count > listTwo.count) {
        a = listTwo
        b = listOne
        result = "superlist"
      }

      for (idx in 0..(b.count - a.count)) {
        if (listsEqual(a, b[idx...(idx + a.count)])) {
          return result
        }
      }
    }
    return "unequal"
  }

  static listsEqual(a, b) {
    for (idx in 0...a.count) {
      if (a[idx] != b[idx]) {
        return false
      }
    }
    return true
  }
}
