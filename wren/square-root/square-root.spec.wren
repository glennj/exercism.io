import "wren-testie/testie" for Testie, Expect
import "./square-root" for SquareRoot

Testie.test("SquareRoot") { |do, skip|
  do.describe("Square Roots") {
    do.test("root of 1") {
      Expect.value(SquareRoot.squareRoot(1)).toEqual(1)
    }

    do.test("root of 4") {
      Expect.value(SquareRoot.squareRoot(4)).toEqual(2)
    }

    do.test("root of 5") {
      Expect.value(SquareRoot.squareRoot(25)).toEqual(5)
    }

    do.test("root of 81") {
      Expect.value(SquareRoot.squareRoot(81)).toEqual(9)
    }

    do.test("root of 196") {
      Expect.value(SquareRoot.squareRoot(196)).toEqual(14)
    }

    do.test("root of 65025") {
      Expect.value(SquareRoot.squareRoot(65025)).toEqual(255)
    }

  }
}
