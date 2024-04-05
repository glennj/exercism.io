include std/unittest.e 

include grains.ex 

set_test_verbosity(TEST_SHOW_ALL)

test_equal("grains on square 1",square(1),1)
test_equal("grains on square 2",square(2),2)
test_equal("grains on square 3",square(3),4)
test_equal("grains on square 4",square(4),8)
test_equal("grains on square 16",square(16),32768)
test_equal("grains on square 32",square(32),2147483648)
test_equal("grains on square 64",square(64),power(2,63))
test_equal("square 0 raises an exception",square(0),"square must be between 1 and 64")
test_equal("negative square raises an exception",square(-1),"square must be between 1 and 64")
test_equal("square greater than 64 raises an exception",square(65),"square must be between 1 and 64")
test_equal("returns the total number of grains on the board", 18446744073709551615, totalgrains())


test_report() 
