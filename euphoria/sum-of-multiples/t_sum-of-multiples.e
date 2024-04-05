include std/unittest.e

include sum-of-multiples.ex

set_test_verbosity(TEST_SHOW_ALL)
test_equal("no multiples within limit",sum_of_multiples( {3,5},1),0)
test_equal("one factor has multiples within limit",sum_of_multiples( {3,5},4),3)
test_equal("more than one multiple within limit",sum_of_multiples( {3},7),9)
test_equal("more than one factor with multiples within limit",sum_of_multiples( {3,5},10),23)
test_equal("each multiple is only counted once",sum_of_multiples( {3,5},100),2318)
test_equal("a much larger limit",sum_of_multiples( {3,5},1000),233168)
test_equal("three factors",sum_of_multiples( {7,13,17},20),51)
test_equal("factors not relatively prime",sum_of_multiples( {4,6},15),30)
test_equal("some pairs of factors relatively prime and some not",sum_of_multiples( {5,6,8},150),4419)
test_equal("one factor is a multiple of another",sum_of_multiples( {5,25},51),275)
test_equal("much larger factors",sum_of_multiples( {43,47},10000),2203160)
test_equal("all numbers are multiples of 1",sum_of_multiples( {1},100),4950)
test_equal("no factors means an empty sum",sum_of_multiples( {},10000),0)
test_equal("the only multiple of 0 is 0",sum_of_multiples( {0},1),0)
test_equal("the factor 0 does not affect the sum of multiples of other factors",sum_of_multiples( {3,0},4),3)
test_equal("solutions using include-exclude must extend to cardinality greater than 3",sum_of_multiples( {2,3,5,7,11},10000),39614537)

test_report()
