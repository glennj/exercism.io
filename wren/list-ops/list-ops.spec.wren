import "wren-testie/testie" for Testie, Expect
import "./list-ops" for ListOps

Testie.test("ListOps") { |do, skip|

  do.describe("wren-specific tests") {
    /* These are "primitive" operations.
     * You can use List methods for these.
     */

    do.test("can be instantiated with a list of elements, and can be converted to a list") {
      var list1 = ListOps.new([1, 2, 3, 4])
      Expect.value(list1.toList).toEqual([1, 2, 3, 4])
    }

    do.test("can be instantiated with no elements") {
      var list1 = ListOps.new()
      Expect.value(list1.toList).toEqual([])
    }

    do.test("can add an item") {
      var list1 = ListOps.new([1, 2])
      list1.add(3)
      Expect.value(list1.toList).toEqual([1, 2, 3])
    }

    do.test("can iterate") {
      var list1 = ListOps.new([1, 2, 3, 4])
      var result = []
      for (element in list1) {
        result.add(element * 10)
      }
      Expect.value(result).toEqual([10, 20, 30, 40])
    }

    do.test("does not mutate given values") {
      var elements = [1, 2, 3]
      var list1 = ListOps.new(elements)
      elements.add(4)
      Expect.value(list1.toList).toEqual([1, 2, 3])
    }

    do.test("does not mutate returned values") {
      var list1 = ListOps.new([4, 5, 6])
      var elements = list1.toList
      elements.add(7)
      Expect.value(list1.toList).toEqual([4, 5, 6])
    }
  }

  /* From here down, stay away from List/Sequence methods */

  do.describe("append entries to a list and return the new list") {
    do.test("empty lists") {
      var list1 = ListOps.new()
      var list2 = ListOps.new()
      list1.addAll(list2)
      Expect.value(list1.toList).toEqual([])
    }

    do.test("list to empty list") {
      var list1 = ListOps.new()
      var list2 = ListOps.new([1, 2, 3, 4])
      list1.addAll(list2)
      Expect.value(list1.toList).toEqual([1, 2, 3, 4])
    }

    do.test("empty list to list") {
      var list1 = ListOps.new([1, 2, 3, 4])
      var list2 = ListOps.new()
      list1.addAll(list2)
      Expect.value(list1.toList).toEqual([1, 2, 3, 4])
    }

    do.test("non-empty lists") {
      var list1 = ListOps.new([1, 2])
      var list2 = ListOps.new([2, 3, 4, 5])
      list1.addAll(list2)
      Expect.value(list1.toList).toEqual([1, 2, 2, 3, 4, 5])
    }

    do.test("non-mutating plus operator") {
      var list1 = ListOps.new([1, 2, 3])
      var list2 = ListOps.new([4, 5, 6])
      var result = list1 + list2
      Expect.value(result.toList).toEqual([1, 2, 3, 4, 5, 6])
      Expect.value(list1.toList).toEqual([1, 2, 3])
      Expect.value(list2.toList).toEqual([4, 5, 6])
    }
  }

  do.describe("concatenate a list of lists") {
    do.test("empty list") {
      var list1 = ListOps.concat([])
      Expect.value(list1.toList).toEqual([])
    }

    do.test("list of lists") {
      var list1 = ListOps.concat([[1, 2], [3], [], [4, 5, 6]])
      Expect.value(list1.toList).toEqual([1, 2, 3, 4, 5, 6])
    }

    do.test("list of nested lists") {
      var list1 = ListOps.concat([[[1], [2]], [[3]], [[]], [[4, 5, 6]]])
      Expect.value(list1.toList).toEqual([[1], [2], [3], [], [4, 5, 6]])
    }
  }

  do.describe("filter list returning only values that satisfy the filter function") {
    do.test("empty list") {
      var list1 = ListOps.new()
      var result = list1.where {|n| n % 2 == 1}
      Expect.value(result.toList).toEqual([])
    }

    do.test("non-empty list") {
      var list1 = ListOps.new([1, 2, 3, 5])
      var result = list1.where {|n| n % 2 == 1}
      Expect.value(result.toList).toEqual([1, 3, 5])
    }
  }

  do.describe("returns the length of a list") {
    do.test("empty list") {
      var list1 = ListOps.new()
      Expect.value(list1.count).toEqual(0)
    }

    do.test("non-empty list") {
      var list1 = ListOps.new([1, 2, 3, 4])
      Expect.value(list1.count).toEqual(4)
    }
  }


  do.describe("return a list of elements whose values equal the list value transformed by the mapping function") {
    do.test("empty list") {
      var list1 = ListOps.new()
      var result = list1.map {|x| x + 1}
      Expect.value(result.toList).toEqual([])
    }

    do.test("non-empty list") {
      var list1 = ListOps.new([1, 3, 5, 7])
      var result = list1.map {|x| x + 1}
      Expect.value(result.toList).toEqual([2, 4, 6, 8])
    }
  }

  do.describe("folds (reduces) the given list from the left with a function") {
    do.test("empty list") {
      var list1 = ListOps.new()
      var product = list1.reduce(2) {|accum, elem| accum * elem}
      Expect.value(product).toEqual(2)
    }

    do.test("direction independent function applied to non-empty list") {
      var list1 = ListOps.new([1, 2, 3, 4])
      var sum = list1.reduce(5) {|accum, elem| accum + elem}
      Expect.value(sum).toEqual(15)
    }

    do.test("direction dependent function applied to non-empty list") {
      var list1 = ListOps.new([1, 2, 3, 4])
      var result = list1.reduce(24) {|accum, elem| elem / accum}
      // that evaluates as: 4 / (3 / (2 / (1 / 24)))
      Expect.value(result).toEqual(64)
    }
  }


  do.describe("folds (reduces) the given list from the right with a function") {
    // Note the order of the arguments to the given functions!

    do.test("empty list") {
      var list1 = ListOps.new()
      var product = list1.reduceRight(2) {|elem, accum| accum * elem}
      Expect.value(product).toEqual(2)
    }

    do.test("direction independent function applied to non-empty list") {
      var list1 = ListOps.new([1, 2, 3, 4])
      var sum = list1.reduceRight(5) {|elem, accum| accum + elem}
      Expect.value(sum).toEqual(15)
    }

    do.test("direction dependent function applied to non-empty list") {
      var list1 = ListOps.new([1, 2, 3, 4])
      var result = list1.reduceRight(24) {|elem, accum| elem / accum}
      // that evaluates as: 1 / (2 / (3 / (4 / 24)))
      Expect.value(result).toEqual(9)
    }
  }

  do.describe("reverse the elements of the list") {
    do.test("empty list") {
      var list1 = ListOps.new()
      Expect.value(list1.reverse().toList).toEqual([])
    }

    do.test("non-empty list") {
      var list1 = ListOps.new([1, 3, 5, 7])
      Expect.value(list1.reverse().toList).toEqual([7, 5, 3, 1])
    }

    do.test("list of lists is not flattened") {
      var list1 = ListOps.new([[1, 2], [3], [], [4, 5, 6]])
      Expect.value(list1.reverse().toList).toEqual([[4, 5, 6], [], [3], [1, 2]])
    }
  }
}
