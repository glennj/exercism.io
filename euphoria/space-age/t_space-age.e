include std/unittest.e 
include std/math.e

include space-age.ex 

set_test_verbosity(TEST_SHOW_ALL)

public function isClose(atom val1, atom val2)
  return abs(val2 - val1) <= 0.01
end function 

test_true("age on Earth", isClose(ageOn("Earth", 1000000000), 31.69))
test_true("age on Mercury", isClose(ageOn("Mercury", 2134835688), 280.88))
test_true("age on Venus", isClose(ageOn("Venus", 189839836), 9.78))
test_true("age on Mars", isClose(ageOn("Mars", 2129871239), 35.88))
test_true("age on Jupiter", isClose(ageOn("Jupiter", 901876382), 2.41))
test_true("age on Saturn", isClose(ageOn("Saturn", 2000000000), 2.15))
test_true("age on Uranus", isClose(ageOn("Uranus", 1210123456), 0.46))
test_true("age on Neptune", isClose(ageOn("Neptune", 1821023456), 0.35))
test_false("invalid planet causes error", ageOn("Sun", 680804807))

test_report() 
