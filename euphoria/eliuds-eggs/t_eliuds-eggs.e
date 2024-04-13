include std/unittest.e 

include eliuds-eggs.ex 

set_test_verbosity(TEST_SHOW_ALL)

test_equal("0 eggs", 0, eggCount(0))
test_equal("1 egg", 1, eggCount(16))
test_equal("4 eggs", 4, eggCount(89))
test_equal("13 eggs", 13, eggCount(2000000000))

test_report() 
