import "./dominoes" for Dominoes
import "wren-testie/testie" for Testie, Expect

Testie.test("Dominoes") { |do, skip|
  do.test("empty input = empty output") {
    var dominoes = []
    Expect.value(Dominoes.canChain(dominoes)).toBe(true)
  }

  do.test("singleton input = singleton output") {
    var dominoes = [[1, 1]]
    Expect.value(Dominoes.canChain(dominoes)).toBe(true)
  }

  do.test("singleton that can't be chained") {
    var dominoes = [[1, 2]]
    Expect.value(Dominoes.canChain(dominoes)).toBe(false)
  }

  do.test("three elements") {
    var dominoes = [[1, 2], [3, 1], [2, 3]]
    Expect.value(Dominoes.canChain(dominoes)).toBe(true)
  }

  do.test("can reverse dominoes") {
    var dominoes = [[1, 2], [1, 3], [2, 3]]
    Expect.value(Dominoes.canChain(dominoes)).toBe(true)
  }

  do.test("can't be chained") {
    var dominoes = [[1, 2], [4, 1], [2, 3]]
    Expect.value(Dominoes.canChain(dominoes)).toBe(false)
  }

  do.test("disconnected - simple") {
    var dominoes = [[1, 1], [2, 2]]
    Expect.value(Dominoes.canChain(dominoes)).toBe(false)
  }

  do.test("disconnected - double loop") {
    var dominoes = [[1, 2], [2, 1], [3, 4], [4, 3]]
    Expect.value(Dominoes.canChain(dominoes)).toBe(false)
  }

  do.test("disconnected - single isolated") {
    var dominoes = [[1, 2], [2, 3], [3, 1], [4, 4]]
    Expect.value(Dominoes.canChain(dominoes)).toBe(false)
  }

  do.test("need backtrack") {
    var dominoes = [[1, 2], [2, 3], [3, 1], [2, 4], [2, 4]]
    Expect.value(Dominoes.canChain(dominoes)).toBe(true)
  }

  do.test("separate loops") {
    var dominoes = [[1, 2], [2, 3], [3, 1], [1, 1], [2, 2], [3, 3]]
    Expect.value(Dominoes.canChain(dominoes)).toBe(true)
  }

  do.test("nine elements") {
    var dominoes = [[1, 2], [5, 3], [3, 1], [1, 2], [2, 4], [1, 6], [2, 3], [3, 4], [5, 6]]
    Expect.value(Dominoes.canChain(dominoes)).toBe(true)
  }

  do.test("separate three-domino loops") {
    var dominoes = [[1, 2], [2, 3], [3, 1], [4, 5], [5, 6], [6, 4]]
    Expect.value(Dominoes.canChain(dominoes)).toBe(false)
  }
}
