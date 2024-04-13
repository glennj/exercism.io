include std/unittest.e 

include resistor-color-duo.ex 

set_test_verbosity(TEST_SHOW_ALL)

test_equal("Brown and black", 10, value({"brown", "black"}))
test_equal("Blue and grey", 68, value({"blue", "grey"}))
test_equal("Yellow and violet", 47, value({"yellow", "violet"}))
test_equal("White and red", 92, value({"white", "red"}))
test_equal("Orange and orange", 33, value({"orange", "orange"}))
test_equal("Ignore additional colors", 51, value({"green", "brown", "orange"}))
test_equal("Black and brown, one-digit", 1, value({"black", "brown"}))

test_report() 