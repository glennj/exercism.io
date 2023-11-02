import "./nth-prime" for NthPrime
import "wren-testie/testie" for Testie, Expect

Testie.test("NthPrime.prime()") { |do, skip|
  do.test("first prime") {
    Expect.value(NthPrime.prime(1)).toEqual(2)
  }

  do.test("second prime") {
    Expect.value(NthPrime.prime(2)).toEqual(3)
  }

  do.test("third prime") {
    Expect.value(NthPrime.prime(3)).toEqual(5)
  }

  do.test("sixth prime") {
    Expect.value(NthPrime.prime(6)).toEqual(13)
  }

  do.test("big prime") {
    Expect.value(NthPrime.prime(10001)).toEqual(104743)
  }

  Expect.that {
    NthPrime.prime(0)
  }.abortsWith("there is no zeroth prime")
}
