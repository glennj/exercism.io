func equals(r1, r2) -> bool:
	r1 = r1.reduce()
	r2 = r2.reduce()
	return r1.numer == r2.numer and r1.denom == r2.denom


func test_add_two_positive_rational_numbers(solution_script):
	var rational_one = solution_script.Rational.new(1, 2)
	var rational_two = solution_script.Rational.new(2, 3)
	var want = solution_script.Rational.new(7, 6)
	var got = rational_one.add(rational_two)
	return [equals(got, want), true]


func test_add_a_positive_rational_number_and_a_negative_rational_number(solution_script):
	var rational_one = solution_script.Rational.new(1, 2)
	var rational_two = solution_script.Rational.new(-2, 3)
	var want = solution_script.Rational.new(-1, 6)
	var got = rational_one.add(rational_two)
	return [equals(got, want), true]


func test_add_two_negative_rational_numbers(solution_script):
	var rational_one = solution_script.Rational.new(-1, 2)
	var rational_two = solution_script.Rational.new(-2, 3)
	var want = solution_script.Rational.new(-7, 6)
	var got = rational_one.add(rational_two)
	return [equals(got, want), true]


func test_add_a_rational_number_to_its_additive_inverse(solution_script):
	var rational_one = solution_script.Rational.new(1, 2)
	var rational_two = solution_script.Rational.new(-1, 2)
	var want = solution_script.Rational.new(0, 1)
	var got = rational_one.add(rational_two)
	return [equals(got, want), true]


func test_subtract_two_positive_rational_numbers(solution_script):
	var rational_one = solution_script.Rational.new(1, 2)
	var rational_two = solution_script.Rational.new(2, 3)
	var want = solution_script.Rational.new(-1, 6)
	var got = rational_one.sub(rational_two)
	return [equals(got, want), true]


func test_subtract_a_positive_rational_number_and_a_negative_rational_number(solution_script):
	var rational_one = solution_script.Rational.new(1, 2)
	var rational_two = solution_script.Rational.new(-2, 3)
	var want = solution_script.Rational.new(7, 6)
	var got = rational_one.sub(rational_two)
	return [equals(got, want), true]


func test_subtract_two_negative_rational_numbers(solution_script):
	var rational_one = solution_script.Rational.new(-1, 2)
	var rational_two = solution_script.Rational.new(-2, 3)
	var want = solution_script.Rational.new(1, 6)
	var got = rational_one.sub(rational_two)
	return [equals(got, want), true]


func test_subtract_a_rational_number_from_itself(solution_script):
	var rational_one = solution_script.Rational.new(1, 2)
	var rational_two = solution_script.Rational.new(1, 2)
	var want = solution_script.Rational.new(0, 1)
	var got = rational_one.sub(rational_two)
	return [equals(got, want), true]


func test_multiply_two_positive_rational_numbers(solution_script):
	var rational_one = solution_script.Rational.new(1, 2)
	var rational_two = solution_script.Rational.new(2, 3)
	var want = solution_script.Rational.new(1, 3)
	var got = rational_one.mul(rational_two)
	return [equals(got, want), true]


func test_multiply_a_negative_rational_number_by_a_positive_rational_number(solution_script):
	var rational_one = solution_script.Rational.new(-1, 2)
	var rational_two = solution_script.Rational.new(2, 3)
	var want = solution_script.Rational.new(-1, 3)
	var got = rational_one.mul(rational_two)
	return [equals(got, want), true]


func test_multiply_two_negative_rational_numbers(solution_script):
	var rational_one = solution_script.Rational.new(-1, 2)
	var rational_two = solution_script.Rational.new(-2, 3)
	var want = solution_script.Rational.new(1, 3)
	var got = rational_one.mul(rational_two)
	return [equals(got, want), true]


func test_multiply_a_rational_number_by_its_reciprocal(solution_script):
	var rational_one = solution_script.Rational.new(1, 2)
	var rational_two = solution_script.Rational.new(2, 1)
	var want = solution_script.Rational.new(1, 1)
	var got = rational_one.mul(rational_two)
	return [equals(got, want), true]


func test_multiply_a_rational_number_by_1(solution_script):
	var rational_one = solution_script.Rational.new(1, 2)
	var rational_two = solution_script.Rational.new(1, 1)
	var want = solution_script.Rational.new(1, 2)
	var got = rational_one.mul(rational_two)
	return [equals(got, want), true]


func test_multiply_a_rational_number_by_0(solution_script):
	var rational_one = solution_script.Rational.new(1, 2)
	var rational_two = solution_script.Rational.new(0, 1)
	var want = solution_script.Rational.new(0, 1)
	var got = rational_one.mul(rational_two)
	return [equals(got, want), true]


func test_divide_two_positive_rational_numbers(solution_script):
	var rational_one = solution_script.Rational.new(1, 2)
	var rational_two = solution_script.Rational.new(2, 3)
	var want = solution_script.Rational.new(3, 4)
	var got = rational_one.div(rational_two)
	return [equals(got, want), true]


func test_divide_a_positive_rational_number_by_a_negative_rational_number(solution_script):
	var rational_one = solution_script.Rational.new(1, 2)
	var rational_two = solution_script.Rational.new(-2, 3)
	var want = solution_script.Rational.new(-3, 4)
	var got = rational_one.div(rational_two)
	return [equals(got, want), true]


func test_divide_two_negative_rational_numbers(solution_script):
	var rational_one = solution_script.Rational.new(-1, 2)
	var rational_two = solution_script.Rational.new(-2, 3)
	var want = solution_script.Rational.new(3, 4)
	var got = rational_one.div(rational_two)
	return [equals(got, want), true]


func test_divide_a_rational_number_by_1(solution_script):
	var rational_one = solution_script.Rational.new(1, 2)
	var rational_two = solution_script.Rational.new(1, 1)
	var want = solution_script.Rational.new(1, 2)
	var got = rational_one.div(rational_two)
	return [equals(got, want), true]


func test_absolute_value_of_a_positive_rational_number(solution_script):
	var rational = solution_script.Rational.new(1, 2)
	var want = solution_script.Rational.new(1, 2)
	var got = rational.abs()
	return [equals(got, want), true]


func test_absolute_value_of_a_positive_rational_number_with_negative_numerator_and_denominator(solution_script):
	var rational = solution_script.Rational.new(-1, -2)
	var want = solution_script.Rational.new(1, 2)
	var got = rational.abs()
	return [equals(got, want), true]


func test_absolute_value_of_a_negative_rational_number(solution_script):
	var rational = solution_script.Rational.new(-1, 2)
	var want = solution_script.Rational.new(1, 2)
	var got = rational.abs()
	return [equals(got, want), true]


func test_absolute_value_of_a_negative_rational_number_with_negative_denominator(solution_script):
	var rational = solution_script.Rational.new(1, -2)
	var want = solution_script.Rational.new(1, 2)
	var got = rational.abs()
	return [equals(got, want), true]


func test_absolute_value_of_zero(solution_script):
	var rational = solution_script.Rational.new(0, 1)
	var want = solution_script.Rational.new(0, 1)
	var got = rational.abs()
	return [equals(got, want), true]


func test_absolute_value_of_a_rational_number_is_reduced_to_lowest_terms(solution_script):
	var rational = solution_script.Rational.new(2, 4)
	var want = solution_script.Rational.new(1, 2)
	var got = rational.abs()
	return [equals(got, want), true]


func test_raise_a_positive_rational_number_to_a_positive_integer_power(solution_script):
	var rational = solution_script.Rational.new(1, 2)
	var integer = 3
	var want = solution_script.Rational.new(1, 8)
	var got = rational.exprational(integer)
	return [equals(got, want), true]


func test_raise_a_negative_rational_number_to_a_positive_integer_power(solution_script):
	var rational = solution_script.Rational.new(-1, 2)
	var integer = 3
	var want = solution_script.Rational.new(-1, 8)
	var got = rational.exprational(integer)
	return [equals(got, want), true]


func test_raise_a_positive_rational_number_to_a_negative_integer_power(solution_script):
	var rational = solution_script.Rational.new(3, 5)
	var integer = -2
	var want = solution_script.Rational.new(25, 9)
	var got = rational.exprational(integer)
	return [equals(got, want), true]


func test_raise_a_negative_rational_number_to_an_even_negative_integer_power(solution_script):
	var rational = solution_script.Rational.new(-3, 5)
	var integer = -2
	var want = solution_script.Rational.new(25, 9)
	var got = rational.exprational(integer)
	return [equals(got, want), true]


func test_raise_a_negative_rational_number_to_an_odd_negative_integer_power(solution_script):
	var rational = solution_script.Rational.new(-3, 5)
	var integer = -3
	var want = solution_script.Rational.new(-125, 27)
	var got = rational.exprational(integer)
	return [equals(got, want), true]


func test_raise_zero_to_an_integer_power(solution_script):
	var rational = solution_script.Rational.new(0, 1)
	var integer = 5
	var want = solution_script.Rational.new(0, 1)
	var got = rational.exprational(integer)
	return [equals(got, want), true]


func test_raise_one_to_an_integer_power(solution_script):
	var rational = solution_script.Rational.new(1, 1)
	var integer = 4
	var want = solution_script.Rational.new(1, 1)
	var got = rational.exprational(integer)
	return [equals(got, want), true]


func test_raise_a_positive_rational_number_to_the_power_of_zero(solution_script):
	var rational = solution_script.Rational.new(1, 2)
	var integer = 0
	var want = solution_script.Rational.new(1, 1)
	var got = rational.exprational(integer)
	return [equals(got, want), true]


func test_raise_a_negative_rational_number_to_the_power_of_zero(solution_script):
	var rational = solution_script.Rational.new(-1, 2)
	var integer = 0
	var want = solution_script.Rational.new(1, 1)
	var got = rational.exprational(integer)
	return [equals(got, want), true]


func test_raise_a_real_number_to_a_positive_rational_number(solution_script):
	var integer = 8
	var rational = solution_script.Rational.new(4, 3)
	var want = 16.0
	var got = rational.expreal(integer)
	return [is_equal_approx(got, want), true]


func test_raise_a_real_number_to_a_negative_rational_number(solution_script):
	var integer = 9
	var rational = solution_script.Rational.new(-1, 2)
	var want = 0.3333333333333333
	var got = rational.expreal(integer)
	return [is_equal_approx(got, want), true]


func test_raise_a_real_number_to_a_zero_rational_number(solution_script):
	var integer = 2
	var rational = solution_script.Rational.new(0, 1)
	var want = 1.0
	var got = rational.expreal(integer)
	return [is_equal_approx(got, want), true]


func test_reduce_a_positive_rational_number_to_lowest_terms(solution_script):
	var rational = solution_script.Rational.new(2, 4)
	var want = solution_script.Rational.new(1, 2)
	var got = rational.reduce()
	return [equals(got, want), true]


func test_reduce_places_the_minus_sign_on_the_numerator(solution_script):
	var rational = solution_script.Rational.new(3, -4)
	var want = solution_script.Rational.new(-3, 4)
	var got = rational.reduce()
	return [equals(got, want), true]


func test_reduce_a_negative_rational_number_to_lowest_terms(solution_script):
	var rational = solution_script.Rational.new(-4, 6)
	var want = solution_script.Rational.new(-2, 3)
	var got = rational.reduce()
	return [equals(got, want), true]


func test_reduce_a_rational_number_with_a_negative_denominator_to_lowest_terms(solution_script):
	var rational = solution_script.Rational.new(3, -9)
	var want = solution_script.Rational.new(-1, 3)
	var got = rational.reduce()
	return [equals(got, want), true]


func test_reduce_zero_to_lowest_terms(solution_script):
	var rational = solution_script.Rational.new(0, 6)
	var want = solution_script.Rational.new(0, 1)
	var got = rational.reduce()
	return [equals(got, want), true]


func test_reduce_an_integer_to_lowest_terms(solution_script):
	var rational = solution_script.Rational.new(-14, 7)
	var want = solution_script.Rational.new(-2, 1)
	var got = rational.reduce()
	return [equals(got, want), true]


func test_reduce_one_to_lowest_terms(solution_script):
	var rational = solution_script.Rational.new(13, 13)
	var want = solution_script.Rational.new(1, 1)
	var got = rational.reduce()
	return [equals(got, want), true]
