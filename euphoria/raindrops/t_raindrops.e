include std/unittest.e 

include raindrops.ex 

set_test_verbosity(TEST_SHOW_ALL)

test_equal("the sound of 1 is 1" , raindrops(1),  "1" )
test_equal("the sound for 10 is Plang as it has a factor 5" , raindrops(10),  "Plang" )
test_equal("the sound for 105 is PlingPlangPlong as it has factors 3, 5 and 7" , raindrops(105),  "PlingPlangPlong" )
test_equal("the sound for 14 is Plong as it has a factor of 7" , raindrops(14),  "Plong" )
test_equal("the sound for 15 is PlingPlang as it has factors 3 and 5" , raindrops(15),  "PlingPlang" )
test_equal("the sound for 21 is PlingPlong as it has factors 3 and 7" , raindrops(21),  "PlingPlong" )
test_equal("the sound for 25 is Plang as it has a factor 5" , raindrops(25),  "Plang" )
test_equal("the sound for 27 is Pling as it has a factor 3" , raindrops(27),  "Pling" )
test_equal("the sound for 3 is Pling" , raindrops(3),  "Pling" )
test_equal("the sound for 3125 is Plang as it has a factor 5" , raindrops(3125),  "Plang" )
test_equal("the sound for 35 is PlangPlong as it has factors 5 and 7" , raindrops(35),  "PlangPlong" )
test_equal("the sound for 49 is Plong as it has a factor 7" , raindrops(49),  "Plong" )
test_equal("the sound for 5 is Plang" , raindrops(5),  "Plang" )
test_equal("the sound for 52 is 52" , raindrops(52),  "52" )
test_equal("the sound for 6 is Pling as it has a factor 3" , raindrops(6),  "Pling" )
test_equal("the sound of 7 is Plong" , raindrops(7),  "Plong" )
test_equal("2 to the power 3 does not make a raindrop sound as 3 is the exponent not the base" , raindrops(8),  "8" )
test_equal("the sound of 9 is Pling as it has a factor 3" , raindrops(9),  "Pling" )

test_report() 