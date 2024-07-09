import "./custom-set" for CustomSet
import "wren-testie/testie" for Testie, Expect

Testie.test("CustomSet") { |do, skip|
  do.describe("empty: returns true if the set contains no elements") {
    do.test("sets with no elements are empty") {
      var actual = CustomSet.new().isEmpty
      Expect.value(actual).toBe(true)
    }

    do.test("sets with elements are not empty") {
      var actual = CustomSet.new([1]).isEmpty
      Expect.value(actual).toBe(false)
    }
  }

  do.describe("contains: sets can report if they contain an element") {
    do.test("nothing is contained in an empty set") {
      var actual = CustomSet.new().contains(1)
      Expect.value(actual).toBe(false)
    }

    do.test("when the element is in the set") {
      var actual = CustomSet.new([1, 2, 3]).contains(1)
      Expect.value(actual).toBe(true)
    }

    do.test("when the element is not in the set") {
      var actual = CustomSet.new([1, 2, 3]).contains(4)
      Expect.value(actual).toBe(false)
    }
  }

  do.describe("subset: a set is a subset if all of its elements are contained in the other set") {
    do.test("empty set is a subset of another empty set") {
      var actual = CustomSet.new().subset(CustomSet.new())
      Expect.value(actual).toBe(true)
    }

    do.test("empty set is a subset of non-empty set") {
      var actual = CustomSet.new().subset(CustomSet.new([1]))
      Expect.value(actual).toBe(true)
    }

    do.test("non-empty set is not a subset of empty set") {
      var actual = CustomSet.new([1]).subset(CustomSet.new())
      Expect.value(actual).toBe(false)
    }

    do.test("set is a subset of set with exact same elements") {
      var actual = CustomSet.new([1, 2, 3]).subset(CustomSet.new([1, 2, 3]))
      Expect.value(actual).toBe(true)
    }

    do.test("set is a subset of larger set with same elements") {
      var actual = CustomSet.new([1, 2, 3]).subset(
        CustomSet.new([4, 1, 2, 3])
      )
      Expect.value(actual).toBe(true)
    }

    do.test("set is not a subset of set that does not contain its elements") {
      var actual = CustomSet.new([1, 2, 3]).subset(CustomSet.new([4, 1, 3]))
      Expect.value(actual).toBe(false)
    }
  }

  do.describe("disjoint: sets are disjoint if they share no elements") {
    do.test("the empty set is disjoint with itself") {
      var actual = CustomSet.new().disjoint(CustomSet.new([]))
      Expect.value(actual).toBe(true)
    }

    do.test("empty set is disjoint with non-empty set") {
      var actual = CustomSet.new().disjoint(CustomSet.new([1]))
      Expect.value(actual).toBe(true)
    }

    do.test("non-empty set is disjoint with empty set") {
      var actual = CustomSet.new([1]).disjoint(CustomSet.new([]))
      Expect.value(actual).toBe(true)
    }

    do.test("sets are not disjoint if they share an element") {
      var actual = CustomSet.new([1, 2]).disjoint(CustomSet.new([2, 3]))
      Expect.value(actual).toBe(false)
    }

    do.test("sets are disjoint if they share no elements") {
      var actual = CustomSet.new([1, 2]).disjoint(CustomSet.new([3, 4]))
      Expect.value(actual).toBe(true)
    }
  }

  do.describe("eql: sets with the same elements are equal") {
    do.test("empty sets are equal") {
      var actual = CustomSet.new().eql(CustomSet.new())
      Expect.value(actual).toBe(true)
    }

    do.test("empty set is not equal to non-empty set") {
      var actual = CustomSet.new().eql(CustomSet.new([1, 2, 3]))
      Expect.value(actual).toBe(false)
    }

    do.test("non-empty set is not equal to empty set") {
      var actual = CustomSet.new([1, 2, 3]).eql(CustomSet.new())
      Expect.value(actual).toBe(false)
    }

    do.test("sets with the same elements are equal") {
      var actual = CustomSet.new([1, 2]).eql(CustomSet.new([2, 1]))
      Expect.value(actual).toBe(true)
    }

    do.test("sets with different elements are not equal") {
      var actual = CustomSet.new([1, 2, 3]).eql(CustomSet.new([1, 2, 4]))
      Expect.value(actual).toBe(false)
    }

    do.test("set is not equal to larger set with same elements") {
      var actual = CustomSet.new([1, 2, 3]).eql(CustomSet.new([1, 2, 3, 4]))
      Expect.value(actual).toBe(false)
    }

    do.test("set is equal to a set constructed from an array with duplicates") {
      var actual = CustomSet.new([1]).eql(CustomSet.new([1, 1]))
      Expect.value(actual).toBe(true)
    }
  }

  do.describe("add: unique elements can be added to a set") {
    do.test("add to empty set") {
      var actual = CustomSet.new().add(3)
      var expected = CustomSet.new([3])
      Expect.value(actual.eql(expected)).toBe(true)
    }

    do.test("add to non-empty set") {
      var actual = CustomSet.new([1, 2, 4]).add(3)
      var expected = CustomSet.new([1, 2, 3, 4])
      Expect.value(actual.eql(expected)).toBe(true)
    }

    do.test("adding an existing element does not change the set") {
      var actual = CustomSet.new([1, 2, 3]).add(3)
      var expected = CustomSet.new([1, 2, 3])
      Expect.value(actual.eql(expected)).toBe(true)
    }
  }

  do.describe("intersection: returns a set of all shared elements") {
    do.test("intersection of two empty sets is an empty set") {
      var actual = CustomSet.new().intersection(CustomSet.new())
      var expected = CustomSet.new()
      Expect.value(actual.eql(expected)).toBe(true)
    }

    do.test("intersection of an empty set and non-empty set is an empty set") {
      var actual = CustomSet.new().intersection(CustomSet.new([3, 2, 5]))
      var expected = CustomSet.new([])
      Expect.value(actual.eql(expected)).toBe(true)
    }

    do.test("intersection of a non-empty set and an empty set is an empty set") {
      var actual = CustomSet.new([1, 2, 3, 4]).intersection(
        CustomSet.new([])
      )
      var expected = CustomSet.new([])
      Expect.value(actual.eql(expected)).toBe(true)
    }

    do.test("intersection of two sets with no shared elements is an empty set") {
      var actual = CustomSet.new([1, 2, 3]).intersection(
        CustomSet.new([4, 5, 6])
      )
      var expected = CustomSet.new([])
      Expect.value(actual.eql(expected)).toBe(true)
    }

    do.test("intersection of two sets with shared elements is a set of the shared elements") {
      var actual = CustomSet.new([1, 2, 3, 4]).intersection(
        CustomSet.new([3, 2, 5])
      )
      var expected = CustomSet.new([2, 3])
      Expect.value(actual.eql(expected)).toBe(true)
    }
  }

  do.describe("difference of a set is a set of all elements that are only in the first set") {
    do.test("difference of two empty sets is an empty set") {
      var actual = CustomSet.new().difference(CustomSet.new())
      var expected = CustomSet.new()
      Expect.value(actual.eql(expected)).toBe(true)
    }

    do.test("difference of empty set and non-empty set is an empty set") {
      var actual = CustomSet.new().difference(CustomSet.new([3, 2, 5]))
      var expected = CustomSet.new()
      Expect.value(actual.eql(expected)).toBe(true)
    }

    do.test("difference of a non-empty set and an empty set is the non-empty set") {
      var actual = CustomSet.new([1, 2, 3, 4]).difference(CustomSet.new())
      var expected = CustomSet.new([1, 2, 3, 4])
      Expect.value(actual.eql(expected)).toBe(true)
    }

    do.test("difference of two non-empty sets is a set of elements that are only in the first set") {
      var actual = CustomSet.new([3, 2, 1]).difference(CustomSet.new([2, 4]))
      var expected = CustomSet.new([1, 3])
      Expect.value(actual.eql(expected)).toBe(true)
    }

    do.test("difference removes all duplicates in the first set") {
      var actual = CustomSet.new([1, 1]).difference(CustomSet.new([1]))
      var expected = CustomSet.new([])
      Expect.value(actual.eql(expected)).toBe(true)
    }
  }

  do.describe("union: returns a set of all elements in either set") {
    do.test("union of empty sets is an empty set") {
      var actual = CustomSet.new().union(CustomSet.new())
      var expected = CustomSet.new()
      Expect.value(actual.eql(expected)).toBe(true)
    }

    do.test("union of an empty set and non-empty set is the non-empty set") {
      var actual = CustomSet.new().union(CustomSet.new([2]))
      var expected = CustomSet.new([2])
      Expect.value(actual.eql(expected)).toBe(true)
    }

    do.test("union of a non-empty set and empty set is the non-empty set") {
      var actual = CustomSet.new([1, 3]).union(CustomSet.new())
      var expected = CustomSet.new([1, 3])
      Expect.value(actual.eql(expected)).toBe(true)
    }

    do.test("union of non-empty sets contains all unique elements") {
      var actual = CustomSet.new([1, 3]).union(CustomSet.new([2, 3]))
      var expected = CustomSet.new([1, 2, 3])
      Expect.value(actual.eql(expected)).toBe(true)
    }
  }

  do.describe("sets can return their items as a List") {
    do.test("toList") {
      var set = CustomSet.new([42, "Zaphod", 3.14])
      var expected = [3.14, 42, "Zaphod"]
      Expect.value(set.toList).toIncludeSameItemsAs(expected)
    }
  }

  do.describe("iterating") {
    do.test("iterating over the items of a set") {
      // refer to https://wren.io/control-flow.html#the-iterator-protocol
      var set = CustomSet.new([3, 2, 1, 3])
      var result = []
      for (item in set) {
        result.add(item * item)
      }
      Expect.value(result).toIncludeSameItemsAs([1, 4, 9])
    }
  }
}
