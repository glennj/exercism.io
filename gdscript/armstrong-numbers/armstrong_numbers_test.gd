func test_zero_is_an_armstrong_number(solution_script):
	var value    = 0
	var expected = true
	return [solution_script.is_armstrong_number(value), expected]


func test_single_digit_numbers_are_armstrong_numbers(solution_script):
	var value    = 5
	var expected = true
	return [solution_script.is_armstrong_number(value), expected]


func test_there_are_no_two_digit_armstrong_numbers(solution_script):
	var value    = 10
	var expected = false
	return [solution_script.is_armstrong_number(value), expected]


func test_three_digit_number_that_is_an_armstrong_number(solution_script):
	var value    = 153
	var expected = true
	return [solution_script.is_armstrong_number(value), expected]


func test_three_digit_number_that_is_not_an_armstrong_number(solution_script):
	var value    = 100
	var expected = false
	return [solution_script.is_armstrong_number(value), expected]


func test_four_digit_number_that_is_an_armstrong_number(solution_script):
	var value    = 9474
	var expected = true
	return [solution_script.is_armstrong_number(value), expected]


func test_four_digit_number_that_is_not_an_armstrong_number(solution_script):
	var value    = 9475
	var expected = false
	return [solution_script.is_armstrong_number(value), expected]


func test_seven_digit_number_that_is_an_armstrong_number(solution_script):
	var value    = 9926315
	var expected = true
	return [solution_script.is_armstrong_number(value), expected]


func test_seven_digit_number_that_is_not_an_armstrong_number(solution_script):
	var value    = 9926314
	var expected = false
	return [solution_script.is_armstrong_number(value), expected]

