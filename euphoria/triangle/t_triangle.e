include std/unittest.e 

include triangle.ex 

set_test_verbosity(TEST_SHOW_ALL)

procedure test_aquel(sequence name, object outcome, object expected) 
    test_equal(name,expected,outcome)
end procedure
test_aquel("all sides are equal",is_equilateral({2,2,2}),1)
test_aquel("any side is unequal",is_equilateral({2,3,2}),0)
test_aquel("no sides are equal",is_equilateral({5,4,6}),0)
test_aquel("all zero sides is not a triangle",is_equilateral({0,0,0}),0)
test_aquel("sides may be floats",is_equilateral({0.50000,0.50000,0.50000}),1)
test_aquel("last two sides are equal",is_isosceles({3,4,4}),1)
test_aquel("first two sides are equal",is_isosceles({4,4,3}),1)
test_aquel("first and last sides are equal",is_isosceles({4,3,4}),1)
test_aquel("equilateral triangles are also isosceles",is_isosceles({4,4,4}),1)
test_aquel("no sides are equal",is_isosceles({2,3,4}),0)
test_aquel("first triangle inequality violation",is_isosceles({1,1,3}),0)
test_aquel("second triangle inequality violation",is_isosceles({1,3,1}),0)
test_aquel("third triangle inequality violation",is_isosceles({3,1,1}),0)
test_aquel("sides may be floats",is_isosceles({0.50000,0.40000,0.50000}),1)
test_aquel("no sides are equal",is_scalene({5,4,6}),1)
test_aquel("all sides are equal",is_scalene({4,4,4}),0)
test_aquel("first and second sides are equal",is_scalene({4,4,3}),0)
test_aquel("first and third sides are equal",is_scalene({3,4,3}),0)
test_aquel("second and third sides are equal",is_scalene({4,3,3}),0)
test_aquel("may not violate triangle inequality",is_scalene({7,3,2}),0)
test_aquel("sides may be floats",is_scalene({0.50000,0.40000,0.60000}),1)

test_report()
