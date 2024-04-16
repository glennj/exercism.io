include std/unittest.e

include difference-of-squares.ex

set_test_verbosity(TEST_SHOW_ALL)

-- Square the sum of the numbers up to the given number

test_equal("square of sum 1", 1, squareOfSum(1))
test_equal("square of sum 5", 225, squareOfSum(5))
test_equal("square of sum 100", 25_502_500, squareOfSum(100))

-- Sum the squares of the numbers up to the given number

test_equal("sum of squares 1", 1, sumOfSquares(1))
test_equal("sum of squares 5", 55, sumOfSquares(5))
test_equal("sum of squares 100", 338_350, sumOfSquares(100))

-- Subtract sum of squares from square of sums

test_equal("difference of squares 1", 0, differenceOfSquares(1))
test_equal("difference of squares 5", 170, differenceOfSquares(5))
test_equal("difference of squares 100", 25_164_150, differenceOfSquares(100))

test_report()
