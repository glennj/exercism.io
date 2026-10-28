func test_single_bit_one_to_decimal(solution_script):
	var got = solution_script.rebase(2, [1], 10)
	var want = [1]
	return [got, want]


func test_binary_to_single_decimal(solution_script):
	var got = solution_script.rebase(2, [1, 0, 1], 10)
	var want = [5]
	return [got, want]


func test_single_decimal_to_binary(solution_script):
	var got = solution_script.rebase(10, [5], 2)
	var want = [1, 0, 1]
	return [got, want]


func test_binary_to_multiple_decimal(solution_script):
	var got = solution_script.rebase(2, [1, 0, 1, 0, 1, 0], 10)
	var want = [4, 2]
	return [got, want]


func test_decimal_to_binary(solution_script):
	var got = solution_script.rebase(10, [4, 2], 2)
	var want = [1, 0, 1, 0, 1, 0]
	return [got, want]


func test_trinary_to_hexadecimal(solution_script):
	var got = solution_script.rebase(3, [1, 1, 2, 0], 16)
	var want = [2, 10]
	return [got, want]


func test_hexadecimal_to_trinary(solution_script):
	var got = solution_script.rebase(16, [2, 10], 3)
	var want = [1, 1, 2, 0]
	return [got, want]


func test_15_bit_integer(solution_script):
	var got = solution_script.rebase(97, [3, 46, 60], 73)
	var want = [6, 10, 45]
	return [got, want]


func test_empty_list(solution_script):
	var got = solution_script.rebase(2, [], 10)
	var want = [0]
	return [got, want]


func test_single_zero(solution_script):
	var got = solution_script.rebase(10, [0], 2)
	var want = [0]
	return [got, want]


func test_multiple_zeros(solution_script):
	var got = solution_script.rebase(10, [0, 0, 0], 2)
	var want = [0]
	return [got, want]


func test_leading_zeros(solution_script):
	var got = solution_script.rebase(7, [0, 6, 0], 10)
	var want = [4, 2]
	return [got, want]


func test_input_base_is_one(solution_script):
	var got = solution_script.rebase(1, [0], 10)
	var want = null
	return [got, want]


func test_input_base_is_zero(solution_script):
	var got = solution_script.rebase(0, [], 10)
	var want = null
	return [got, want]


func test_input_base_is_negative(solution_script):
	var got = solution_script.rebase(-2, [1], 10)
	var want = null
	return [got, want]


func test_negative_digit(solution_script):
	var got = solution_script.rebase(2, [1, -1, 1, 0, 1, 0], 10)
	var want = null
	return [got, want]


func test_invalid_positive_digit(solution_script):
	var got = solution_script.rebase(2, [1, 2, 1, 0, 1, 0], 10)
	var want = null
	return [got, want]


func test_output_base_is_one(solution_script):
	var got = solution_script.rebase(2, [1, 0, 1, 0, 1, 0], 1)
	var want = null
	return [got, want]


func test_output_base_is_zero(solution_script):
	var got = solution_script.rebase(10, [7], 0)
	var want = null
	return [got, want]


func test_output_base_is_negative(solution_script):
	var got = solution_script.rebase(2, [1], -7)
	var want = null
	return [got, want]


func test_both_bases_are_negative(solution_script):
	var got = solution_script.rebase(-2, [1], -7)
	var want = null
	return [got, want]
