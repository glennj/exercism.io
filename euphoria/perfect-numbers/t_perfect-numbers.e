include std/unittest.e

include perfect-numbers.ex

set_test_verbosity(TEST_SHOW_ALL)

test_equal("Smallest perfect number is classified correctly",
           "perfect", classify(6))
test_equal("Medium perfect number is classified correctly",
           "perfect", classify(28))
test_equal("Large perfect number is classified correctly",
           "perfect", classify(33550336))

test_equal("Smallest abundant number is classified correctly",
           "abundant", classify(12))
test_equal("Medium abundant number is classified correctly",
           "abundant", classify(30))
test_equal("Large abundant number is classified correctly",
           "abundant", classify(33550335))

test_equal("Smallest deficient number is classified correctly",
           "deficient", classify(4))
test_equal("Medium deficient number is classified correctly",
           "deficient", classify(32))
test_equal("Large deficient number is classified correctly",
           "deficient", classify(33550337))
test_equal("Edge case (no factors other than itself) is classified correctly",
           "deficient", classify(1))

test_false("Zero is rejected (as it is not a positive integer)", 
           classify(0))
test_false("Negative integer is rejected (as it is not a positive integer)",
           classify(-1))

test_report()
