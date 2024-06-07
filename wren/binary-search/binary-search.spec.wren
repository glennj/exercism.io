import "./binary-search" for BinarySearch
import "wren-testie/testie" for Testie, Expect

Testie.test("BinarySearch") { |do, skip|
  do.test("finds a value in a list with one element") {
    var actual = BinarySearch.find([6], 6)
    var expected = 0
    Expect.value(actual).toEqual(expected)
  }

  do.test("finds a value in the middle of a list") {
    var actual = BinarySearch.find([1, 3, 4, 6, 8, 9, 11], 6)
    var expected = 3
    Expect.value(actual).toEqual(expected)
  }

  do.test("finds a value at the beginning of a list") {
    var actual = BinarySearch.find([1, 3, 4, 6, 8, 9, 11], 1)
    var expected = 0
    Expect.value(actual).toEqual(expected)
  }

  do.test("finds a value at the end of a list") {
    var actual = BinarySearch.find([1, 3, 4, 6, 8, 9, 11], 11)
    var expected = 6
    Expect.value(actual).toEqual(expected)
  }

  do.test("finds a value in a list of odd length") {
    var actual = BinarySearch.find([1, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 634], 144)
    var expected = 9
    Expect.value(actual).toEqual(expected)
  }

  do.test("finds a value in a list of even length") {
    var actual = BinarySearch.find([1, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377], 21)
    var expected = 5
    Expect.value(actual).toEqual(expected)
  }

  do.test("identifies that a value is not included in the list") {
    Expect.that {
      BinarySearch.find([1, 3, 4, 6, 8, 9, 11], 7)
    }.abortsWith("value not in list")
  }

  do.test("a value smaller than the list's smallest value is not found") {
    Expect.that {
      BinarySearch.find([1, 3, 4, 6, 8, 9, 11], 0)
    }.abortsWith("value not in list")
  }

  do.test("a value larger than the list's largest value is not found") {
    Expect.that {
      BinarySearch.find([1, 3, 4, 6, 8, 9, 11], 13)
    }.abortsWith("value not in list")
  }

  do.test("nothing is found in an empty list") {
    Expect.that {
      BinarySearch.find([], 1)
    }.abortsWith("value not in list")
  }

  do.test("nothing is found when the left and right bounds cross") {
    Expect.that {
      BinarySearch.find([1, 2], 0)
    }.abortsWith("value not in list")
  }
}

