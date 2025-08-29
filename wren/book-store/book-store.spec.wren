import "./book-store" for BookStore
import "wren-testie/testie" for Testie, Expect

Testie.test("BookStore") { |do, skip|
  do.test("Only a single book") {
    var basket = [1]
    Expect.value(BookStore.total(basket)).toEqual(800)
  }

  do.test("Two of the same book") {
    var basket = [2, 2]
    Expect.value(BookStore.total(basket)).toEqual(1600)
  }

  do.test("Empty basket") {
    var basket = []
    Expect.value(BookStore.total(basket)).toEqual(0)
  }

  do.test("Two different books") {
    var basket = [1, 2]
    Expect.value(BookStore.total(basket)).toEqual(1520)
  }

  do.test("Three different books") {
    var basket = [1, 2, 3]
    Expect.value(BookStore.total(basket)).toEqual(2160)
  }

  do.test("Four different books") {
    var basket = [1, 2, 3, 4]
    Expect.value(BookStore.total(basket)).toEqual(2560)
  }

  do.test("Five different books") {
    var basket = [1, 2, 3, 4, 5]
    Expect.value(BookStore.total(basket)).toEqual(3000)
  }

  do.test("Two groups of four is cheaper than group of five plus group of three") {
    var basket = [1, 1, 2, 2, 3, 3, 4, 5]
    Expect.value(BookStore.total(basket)).toEqual(5120)
  }

  do.test("Two groups of four is cheaper than groups of five and three") {
    var basket = [1, 1, 2, 3, 4, 4, 5, 5]
    Expect.value(BookStore.total(basket)).toEqual(5120)
  }

  do.test("Group of four plus group of two is cheaper than two groups of three") {
    var basket = [1, 1, 2, 2, 3, 4]
    Expect.value(BookStore.total(basket)).toEqual(4080)
  }

  do.test("Two each of first four books and one copy each of rest") {
    var basket = [1, 1, 2, 2, 3, 3, 4, 4, 5]
    Expect.value(BookStore.total(basket)).toEqual(5560)
  }

  do.test("Two copies of each book") {
    var basket = [1, 1, 2, 2, 3, 3, 4, 4, 5, 5]
    Expect.value(BookStore.total(basket)).toEqual(6000)
  }

  do.test("Three copies of first book and two each of remaining") {
    var basket = [1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 1]
    Expect.value(BookStore.total(basket)).toEqual(6800)
  }

  do.test("Three each of first two books and two each of remaining books") {
    var basket = [1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 1, 2]
    Expect.value(BookStore.total(basket)).toEqual(7520)
  }

  do.test("Four groups of four are cheaper than two groups each of five and three") {
    var basket = [1, 1, 2, 2, 3, 3, 4, 5, 1, 1, 2, 2, 3, 3, 4, 5]
    Expect.value(BookStore.total(basket)).toEqual(10240)
  }

  do.test("Check that groups of four are created properly even when there are more groups of three than groups of five") {
    var basket = [1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 5, 5]
    Expect.value(BookStore.total(basket)).toEqual(14560)
  }

  do.test("One group of one and four is cheaper than one group of two and three") {
    var basket = [1, 1, 2, 3, 4]
    Expect.value(BookStore.total(basket)).toEqual(3360)
  }

  do.test("One group of one and two plus three groups of four is cheaper than one group of each size") {
    var basket = [1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5]
    Expect.value(BookStore.total(basket)).toEqual(10000)
  }
}
