import "./perfect-numbers" for PerfectNumbers
import "wren-testie/testie" for Testie, Expect

Testie.test("PerfectNumbers.classify()") { |do, skip|
  do.test("Smallest perfect number is classified correctly") {
    Expect.value(PerfectNumbers.classify(6)).toEqual("perfect")
  }

  do.test("Medium perfect number is classified correctly") {
    Expect.value(PerfectNumbers.classify(28)).toEqual("perfect")
  }

  do.test("Large perfect number is classified correctly") {
    Expect.value(PerfectNumbers.classify(33550336)).toEqual("perfect")
  }

  do.test("Smallest abundant number is classified correctly") {
    Expect.value(PerfectNumbers.classify(12)).toEqual("abundant")
  }

  do.test("Medium abundant number is classified correctly") {
    Expect.value(PerfectNumbers.classify(30)).toEqual("abundant")
  }

  do.test("Large abundant number is classified correctly") {
    Expect.value(PerfectNumbers.classify(33550335)).toEqual("abundant")
  }

  do.test("Smallest prime deficient number is classified correctly") {
    Expect.value(PerfectNumbers.classify(2)).toEqual("deficient")
  }

  do.test("Smallest non-prime deficient number is classified correctly") {
    Expect.value(PerfectNumbers.classify(4)).toEqual("deficient")
  }

  do.test("Medium deficient number is classified correctly") {
    Expect.value(PerfectNumbers.classify(32)).toEqual("deficient")
  }

  do.test("Large deficient number is classified correctly") {
    Expect.value(PerfectNumbers.classify(33550337)).toEqual("deficient")
  }

  do.test("Edge case (no factors other than itself) is classified correctly") {
    Expect.value(PerfectNumbers.classify(1)).toEqual("deficient")
  }

  Expect.that {
    PerfectNumbers.classify(0)
  }.abortsWith("Classification is only possible for positive integers.")

  Expect.that {
    PerfectNumbers.classify(-1)
  }.abortsWith("Classification is only possible for positive integers.")
}
