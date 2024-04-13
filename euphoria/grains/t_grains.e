include std/unittest.e 

include grains.ex 

set_test_verbosity(TEST_SHOW_ALL)

test_equal("grains on square 1",1,square(1))
test_equal("grains on square 2",2,square(2))
test_equal("grains on square 3",4,square(3))
test_equal("grains on square 4",8,square(4))
test_equal("grains on square 16",32768,square(16))
test_equal("grains on square 32",2_147_483_648,square(32))
test_equal("grains on square 64",9_223_372_036_854_775_808,square(64))
test_equal("square 0 raises an exception","square must be between 1 and 64",square(0))
test_equal("negative square raises an exception","square must be between 1 and 64",square(-1))
test_equal("square greater than 64 raises an exception","square must be between 1 and 64",square(65))
test_equal("returns the total number of grains on the board", 18_446_744_073_709_551_615, totalgrains())


test_report() 
