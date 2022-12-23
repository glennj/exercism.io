import "wren-testie/testie" for Testie, Expect
import "./change" for Change

Testie.test("Change") { |do, skip|
  do.test("change for 1 cent") {
    var result = Change.findMinimumCoins(1, [1, 5, 10, 25])
    Expect.value(result).toEqual([1])
  }

  skip.test("single coin change") {
    var result = Change.findMinimumCoins(25, [1, 5, 10, 25, 100])
    Expect.value(result).toEqual([25])
  }

  skip.test("multiple coin change") {
    var result = Change.findMinimumCoins(15, [1, 5, 10, 25, 100])
    Expect.value(result).toEqual([5, 10])
  }

  skip.test("change with Lilliputian Coins") {
    var result = Change.findMinimumCoins(23, [1, 4, 15, 20, 50])
    Expect.value(result).toEqual([4, 4, 15])
  }

  skip.test("change with Lower Elbonia Coins") {
    var result = Change.findMinimumCoins(63, [1, 5, 10, 21, 25])
    Expect.value(result).toEqual([21, 21, 21])
  }

  skip.test("large target values") {
    var result = Change.findMinimumCoins(999, [1, 2, 5, 10, 20, 50, 100])
    Expect.value(result).toEqual([2, 2, 5, 20, 20, 50, 100, 100, 100, 100, 100, 100, 100, 100, 100])
  }

  skip.test("possible change without unit coins available") {
    var result = Change.findMinimumCoins(21, [2, 5, 10, 20, 50])
    Expect.value(result).toEqual([2, 2, 2, 5, 10])
  }

  skip.test("another possible change without unit coins available") {
    var result = Change.findMinimumCoins(27, [4, 5])
    Expect.value(result).toEqual([4, 4, 4, 5, 5, 5])
  }

  skip.test("no coins make 0 change") {
    var result = Change.findMinimumCoins(0, [1, 5, 10, 21, 25])
    Expect.value(result).toEqual([])
  }

  skip.test("error testing for change smaller than the smallest of coins") {
    Expect.that {
      Change.findMinimumCoins(3, [5, 10])
    }.abortsWith("can't make target with given coins")
  }

  skip.test("error if no combination can add up to target") {
    Expect.that {
      Change.findMinimumCoins(94, [5, 10])
    }.abortsWith("can't make target with given coins")
  }

  skip.test("cannot find negative change values") {
    Expect.that {
      Change.findMinimumCoins(-5, [1, 2, 5])
    }.abortsWith("target can't be negative")
  }
}
