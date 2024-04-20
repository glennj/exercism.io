include std/unittest.e

include armstrong-numbers.ex

set_test_verbosity(TEST_SHOW_ALL)

test_true("Zero is an Armstrong number",
          is_armstrong_number(0))
test_true("Single-digit numbers are Armstrong numbers",
          is_armstrong_number(5))
test_false("There are no two-digit Armstrong numbers",
           is_armstrong_number(10))
test_true("Three-digit number that is an Armstrong number",
          is_armstrong_number(153))
test_false("Three-digit number that is not an Armstrong number",
           is_armstrong_number(100))
test_true("Four-digit number that is an Armstrong number",
          is_armstrong_number(9474))
test_false("Four-digit number that is not an Armstrong number",
           is_armstrong_number(9475))
test_true("Seven-digit number that is an Armstrong number",
          is_armstrong_number(9926315))
test_false("Seven-digit number that is not an Armstrong number",
           is_armstrong_number(9926314))

test_report()
