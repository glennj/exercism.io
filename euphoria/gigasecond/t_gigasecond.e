include std/datetime.e
include std/unittest.e 

include gigasecond.ex 

set_test_verbosity(TEST_SHOW_ALL)

test_equal("date only specification of time",
            new(2043, 1, 1, 1, 46, 40),
            add_gigasecond(new(2011, 4, 25, 0, 0, 0)))
test_equal("second test for date only specification of time",
            new(2009, 2, 19, 1, 46, 40),
            add_gigasecond(new(1977, 6, 13, 0, 0, 0)))
test_equal("third test for date only specification of time",
            new(1991, 3, 27, 1, 46, 40),
            add_gigasecond(new(1959, 7, 19, 0, 0, 0)))
test_equal("full time specified",
            new(2046, 10, 2, 23, 46, 40),
            add_gigasecond(new(2015, 1, 24, 22, 0, 0)))
test_equal("full time with day roll-over",
            new(2046, 10, 3, 1, 46, 39),
            add_gigasecond(new(2015, 1, 24, 23, 59, 59)))

test_report() 
