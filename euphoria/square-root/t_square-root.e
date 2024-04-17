include std/unittest.e

include square-root.ex

set_test_verbosity(TEST_SHOW_ALL)

test_equal("root of 1", 1, squareRoot(1))
test_equal("root of 4", 2, squareRoot(4))
test_equal("root of 25", 5, squareRoot(25))
test_equal("root of 81", 9, squareRoot(81))
test_equal("root of 196", 14, squareRoot(196))
test_equal("root of 65025", 255, squareRoot(65025))

test_report()
