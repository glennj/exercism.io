import "./collatz-conjecture" for Collatz
import "wren-testie/testie" for Testie, Expect

Testie.test("Collatz.steps()") { |do, skip|
  do.test("zero steps for one") {
    Expect.value(Collatz.steps(1)).toEqual(0)
  }

  skip.test("divide if even") {
    Expect.value(Collatz.steps(16)).toEqual(4)
  }

  skip.test("even and odd steps") {
    Expect.value(Collatz.steps(12)).toEqual(9)
  }

  skip.test("large number of even and odd steps") {
    Expect.value(Collatz.steps(1000000)).toEqual(152)
  }

  skip.test("zero is an error") {
    Expect.that {
      Collatz.steps(0)
    }.abortsWith("Only positive numbers are allowed")
  }

  skip.test("negative value is an error") {
    Expect.that {
      Collatz.steps(-15)
    }.abortsWith("Only positive numbers are allowed")
  }
}
