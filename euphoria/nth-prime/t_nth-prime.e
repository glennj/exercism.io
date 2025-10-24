include std/unittest.e

include nth-prime.ex

set_test_verbosity(TEST_SHOW_ALL)

test_equal("first prime", 2, prime(1))
test_equal("second prime", 3, prime(2))
test_equal("sixth prime", 13, prime(6))
test_equal("big prime", 104743, prime(10001))

test_report()