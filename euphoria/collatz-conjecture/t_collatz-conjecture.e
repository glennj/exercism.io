include std/unittest.e

include collatz-conjecture.ex

set_test_verbosity(TEST_SHOW_ALL)

test_equal("zero steps for one",steps(1),0)
test_equal("divide if even",steps(16),4)
test_equal("even and odd steps",steps(12),9)
test_equal("large number of even and odd steps",steps(1000000),152)
test_equal("zero is an error",steps(0),"Only positive numbers are allowed")
test_equal("negative value is an error",steps(-15),"Only positive numbers are allowed")

test_report()