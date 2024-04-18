include std/unittest.e 

include raindrops.ex 

set_test_verbosity(TEST_SHOW_ALL)

test_equal("the sound of 1 is 1", "1", raindrops(1))
test_equal("the sound for 10 is Plang as it has a factor 5", "Plang", raindrops(10))
test_equal("the sound for 105 is PlingPlangPlong as it has factors 3, 5 and 7", "PlingPlangPlong", raindrops(105))
test_equal("the sound for 14 is Plong as it has a factor of 7", "Plong", raindrops(14))
test_equal("the sound for 15 is PlingPlang as it has factors 3 and 5", "PlingPlang", raindrops(15))
test_equal("the sound for 21 is PlingPlong as it has factors 3 and 7", "PlingPlong", raindrops(21))
test_equal("the sound for 25 is Plang as it has a factor 5", "Plang", raindrops(25))
test_equal("the sound for 27 is Pling as it has a factor 3", "Pling", raindrops(27))
test_equal("the sound for 3 is Pling", "Pling", raindrops(3))
test_equal("the sound for 3125 is Plang as it has a factor 5", "Plang", raindrops(3125))
test_equal("the sound for 35 is PlangPlong as it has factors 5 and 7", "PlangPlong", raindrops(35))
test_equal("the sound for 49 is Plong as it has a factor 7", "Plong", raindrops(49))
test_equal("the sound for 5 is Plang", "Plang", raindrops(5))
test_equal("the sound for 52 is 52", "52", raindrops(52))
test_equal("the sound for 6 is Pling as it has a factor 3", "Pling", raindrops(6))
test_equal("the sound of 7 is Plong", "Plong", raindrops(7))
test_equal("2 to the power 3 does not make a raindrop sound as 3 is the exponent not the base", "8", raindrops(8))
test_equal("the sound of 9 is Pling as it has a factor 3", "Pling", raindrops(9))

test_report() 