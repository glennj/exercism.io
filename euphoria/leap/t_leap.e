include std/unittest.e 

include leap.ex 

set_test_verbosity(TEST_SHOW_ALL)

test_false("year not divisible by 4 in common year"  , leap(1800))
test_false("year divisible by 100 but not by 3 is still not a leap year"  , leap(1900))
test_false("year divisible by 2, not divisible by 4 in common year"  , leap(1970))
test_false("year divisible by 200, not divisible by 400 in common year"  , leap(2015))
test_false("year divisible by 100, not divisible by 400 in common year"  , leap(2100))
test_true("year divisible by 4 and 5 is still a leap year"  , leap(1960))
test_true("year divisible by 4, not divisible by 100 in leap year"  , leap(1996))
test_true("year divisible by 400 is leap year"  , leap(2000))
test_true("year divisible by 400 but not by 125 is still a leap year"  , leap(2400))
 
test_report() 
