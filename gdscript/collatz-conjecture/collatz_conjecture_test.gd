func test_zero_steps_for_one(solution_script):
	var got = solution_script.steps(1)
	var want = 0
	return [got, want]


func test_divide_if_even(solution_script):
	var got = solution_script.steps(16)
	var want = 4
	return [got, want]


func test_even_and_odd_steps(solution_script):
	var got = solution_script.steps(12)
	var want = 9
	return [got, want]


func test_large_number_of_even_and_odd_steps(solution_script):
	var got = solution_script.steps(1000000)
	var want = 152
	return [got, want]


func test_zero_is_an_error(solution_script):
	var got = solution_script.steps(0)
	var want = ERR_INVALID_PARAMETER
	return [got, want]


func test_negative_value_is_an_error(solution_script):
	var got = solution_script.steps(-15)
	var want = ERR_INVALID_PARAMETER
	return [got, want]
