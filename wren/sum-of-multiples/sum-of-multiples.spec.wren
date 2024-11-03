import "./sum-of-multiples" for SumOfMultiples
import "wren-testie/testie" for Testie, Expect

Testie.test("SumOfMultiples") { |do, skip|
  do.test("no multiples within limit") {
    Expect.value(SumOfMultiples.sum([3, 5], 1)).toEqual(0)
  }

  skip.test("one factor has multiples within limit") {
    Expect.value(SumOfMultiples.sum([3, 5], 4)).toEqual(3)
  }

  skip.test("more than one multiple within limit") {
    Expect.value(SumOfMultiples.sum([3], 7)).toEqual(9)
  }

  skip.test("more than one factor with multiples within limit") {
    Expect.value(SumOfMultiples.sum([3, 5], 10)).toEqual(23)
  }

  skip.test("each multiple is only counted once") {
    Expect.value(SumOfMultiples.sum([3, 5], 100)).toEqual(2318)
  }

  skip.test("a much larger limit") {
    Expect.value(SumOfMultiples.sum([3, 5], 1000)).toEqual(233168)
  }

  skip.test("three factors") {
    Expect.value(SumOfMultiples.sum([7, 13, 17], 20)).toEqual(51)
  }

  skip.test("factors not relatively prime") {
    Expect.value(SumOfMultiples.sum([4, 6], 15)).toEqual(30)
  }

  skip.test("some pairs of factors relatively prime and some not") {
    Expect.value(SumOfMultiples.sum([5, 6, 8], 150)).toEqual(4419)
  }

  skip.test("one factor is a multiple of another") {
    Expect.value(SumOfMultiples.sum([5, 25], 51)).toEqual(275)
  }

  skip.test("much larger factors") {
    Expect.value(SumOfMultiples.sum([43, 47], 10000)).toEqual(2203160)
  }

  skip.test("all numbers are multiples of 1") {
    Expect.value(SumOfMultiples.sum([1], 100)).toEqual(4950)
  }

  skip.test("no factors means an empty sum") {
    Expect.value(SumOfMultiples.sum([], 10000)).toEqual(0)
  }

  skip.test("the only multiple of 0 is 0") {
    Expect.value(SumOfMultiples.sum([0], 1)).toEqual(0)
  }

  skip.test("the factor 0 does not affect the sum of multiples of other factors") {
    Expect.value(SumOfMultiples.sum([3, 0], 4)).toEqual(3)
  }

  skip.test("solutions using include-exclude must extend to cardinality greater than 3") {
    Expect.value(SumOfMultiples.sum([2, 3, 5, 7, 11], 10000)).toEqual(39614537)
  }
}
