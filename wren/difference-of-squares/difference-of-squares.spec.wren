import "wren-testie/testie" for Testie, Expect
import "./difference-of-squares" for Squares

Testie.test("Difference of Squares") { |do, skip|

  do.test("square of sum to 5") {
    var result = Squares.squareOfSum(5)
    Expect.value(result).toEqual(225)
  }

  do.test("sum of squares to 5") {
    var result = Squares.sumOfSquares(5)
    Expect.value(result).toEqual(55)
  }

  do.test("difference of squares to 5") {
    var result = Squares.differenceOfSquares(5)
    Expect.value(result).toEqual(170)
  }
  
  do.test("square of sum to 10") {
    var result = Squares.squareOfSum(10)
    Expect.value(result).toEqual(3025)
  }

  do.test("sum of squares to 10") {
    var result = Squares.sumOfSquares(10)
    Expect.value(result).toEqual(385)
  }

  do.test("difference of squares to 10") {
    var result = Squares.differenceOfSquares(10)
    Expect.value(result).toEqual(2640)
  }

  do.test("square of sum to 100") {
    var result = Squares.squareOfSum(100)
    Expect.value(result).toEqual(25502500)
  }

  do.test("sum of squares to 100") {
    var result = Squares.sumOfSquares(100)
    Expect.value(result).toEqual(338350)
  }

  do.test("difference of squares to 100") {
    var result = Squares.differenceOfSquares(100)
    Expect.value(result).toEqual(25164150)
  }

}
