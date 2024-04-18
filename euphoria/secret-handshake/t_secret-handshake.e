include std/unittest.e

include secret-handshake.ex

set_test_verbosity(TEST_SHOW_ALL)

test_equal("wink for 1",
           {"wink"},
           commands(1))
test_equal("double blink for 10",
           {"double blink"},
           commands(2))
test_equal("close your eyes for 100",
           {"close your eyes"},
           commands(4))
test_equal("jump for 1000",
           {"jump"},
           commands(8))
test_equal("combine two actions",
           {"wink", "double blink"},
           commands(3))
test_equal("reverse two actions",
           {"double blink", "wink"},
           commands(19))
test_equal("reversing one action gives the same action",
           {"jump"},
           commands(24))
test_equal("reversing no actions still gives no actions",
            {},
            commands(16))
test_equal("all possible actions",
            {"wink", "double blink", "close your eyes", "jump"},
            commands(15))
test_equal("reverse all possible actions",
            {"jump", "close your eyes", "double blink", "wink"},
            commands(31))
test_equal("do nothing for zero",
            {},
            commands(0))

test_report()
