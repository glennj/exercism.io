import "./prime-factors" for PrimeFactors
import "wren-testie/testie" for Testie, Expect

Testie.test("PrimeFactors") { |do, skip|
  do.test("no factors") {
    Expect.value(PrimeFactors.factors(1)).toEqual([])
  }

  skip.test("prime number") {
    Expect.value(PrimeFactors.factors(2)).toEqual([2])
  }

  skip.test("another prime number") {
    Expect.value(PrimeFactors.factors(3)).toEqual([3])
  }

  skip.test("square of a prime") {
    Expect.value(PrimeFactors.factors(9)).toEqual([3, 3])
  }

  skip.test("product of first prime") {
    Expect.value(PrimeFactors.factors(4)).toEqual([2, 2])
  }

  skip.test("cube of a prime") {
    Expect.value(PrimeFactors.factors(8)).toEqual([2, 2, 2])
  }

  skip.test("product of second prime") {
    Expect.value(PrimeFactors.factors(27)).toEqual([3, 3, 3])
  }

  skip.test("product of third prime") {
    Expect.value(PrimeFactors.factors(625)).toEqual([5, 5, 5, 5])
  }

  skip.test("product of first and second prime") {
    Expect.value(PrimeFactors.factors(6)).toEqual([2, 3])
  }

  skip.test("product of primes and non-primes") {
    Expect.value(PrimeFactors.factors(12)).toEqual([2, 2, 3])
  }

  skip.test("product of primes") {
    Expect.value(PrimeFactors.factors(901255)).toEqual([5, 17, 23, 461])
  }

  skip.test("factors include a large prime") {
    Expect.value(PrimeFactors.factors(93819012551)).toEqual([11, 9539, 894119])
  }
}
