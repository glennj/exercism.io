include std/unittest.e 

include resistor-color-trio.ex 

set_test_verbosity(TEST_SHOW_ALL)

test_equal("Orange and orange and black", "33 ohms", value({"orange", "orange", "black"}))
test_equal("Blue and grey and brown", "680 ohms", value({"blue", "grey", "brown"}))
test_equal("Red and black and red", "2 kiloohms", value({"red", "black", "red"}))
test_equal("Green and brown and orange", "51 kiloohms", value({"green", "brown", "orange"}))
test_equal("Yellow and violet and yellow", "470 kiloohms", value({"yellow", "violet", "yellow"}))
test_equal("Blue and violet and blue", "67 megaohms", value({"blue", "violet", "blue"}))
test_equal("Minimum possible value", "0 ohms", value({"black", "black", "black"}))
test_equal("Maximum possible value", "99 gigaohms", value({"white", "white", "white"}))
test_equal("First two colors make an invalid octal number", "8 ohms", value({"black", "grey", "black"}))
test_equal("Ignore extra colors", "650 kiloohms", value({"blue", "green", "yellow", "orange"}))

test_report()