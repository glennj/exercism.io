open Test
open DifferenceOfSquares

let equal = (~message=?, a, b) => assertion(~message?, ~operator="equal", (a, b) => a == b, a, b)

test("square of sum 1", () => {equal(~message="square of sum 1", squareOfSum(1), 1)})

test("square of sum 5", () => {equal(~message="square of sum 5", squareOfSum(5), 225)})

test("square of sum 100", () => {equal(~message="square of sum 100", squareOfSum(100), 25502500)})

test("sum of squares 1", () => {equal(~message="sum of squares 1", sumOfSquares(1), 1)})

test("sum of squares 5", () => {equal(~message="sum of squares 5", sumOfSquares(5), 55)})

test("sum of squares 100", () => {equal(~message="sum of squares 100", sumOfSquares(100), 338350)})

test("difference of squares 1", () => {
  equal(~message="difference of squares 1", differenceOfSquares(1), 0)
})

test("difference of squares 5", () => {
  equal(~message="difference of squares 5", differenceOfSquares(5), 170)
})

test("difference of squares 100", () => {
  equal(~message="difference of squares 100", differenceOfSquares(100), 25164150)
})
