func test_zero(solution_script):
	var want = [0]

	var input = [0]
	var got = solution_script.encode(input)
	return [got, want]


func test_arbitrary_single_byte(solution_script):
	var want = [64]

	var input = [64]
	var got = solution_script.encode(input)
	return [got, want]


func test_asymmetric_single_byte(solution_script):
	var want = [83]

	var input = [83]
	var got = solution_script.encode(input)
	return [got, want]


func test_largest_single_byte(solution_script):
	var want = [127]

	var input = [127]
	var got = solution_script.encode(input)
	return [got, want]


func test_smallest_double_byte(solution_script):
	var want = [129, 0]

	var input = [128]
	var got = solution_script.encode(input)
	return [got, want]


func test_arbitrary_double_byte(solution_script):
	var want = [192, 0]

	var input = [8192]
	var got = solution_script.encode(input)
	return [got, want]


func test_asymmetric_double_byte(solution_script):
	var want = [129, 45]

	var input = [173]
	var got = solution_script.encode(input)
	return [got, want]


func test_largest_double_byte(solution_script):
	var want = [255, 127]

	var input = [16383]
	var got = solution_script.encode(input)
	return [got, want]


func test_smallest_triple_byte(solution_script):
	var want = [129, 128, 0]

	var input = [16384]
	var got = solution_script.encode(input)
	return [got, want]


func test_arbitrary_triple_byte(solution_script):
	var want = [192, 128, 0]

	var input = [1048576]
	var got = solution_script.encode(input)
	return [got, want]


func test_asymmetric_triple_byte(solution_script):
	var want = [135, 171, 28]

	var input = [120220]
	var got = solution_script.encode(input)
	return [got, want]


func test_largest_triple_byte(solution_script):
	var want = [255, 255, 127]

	var input = [2097151]
	var got = solution_script.encode(input)
	return [got, want]


func test_smallest_quadruple_byte(solution_script):
	var want = [129, 128, 128, 0]

	var input = [2097152]
	var got = solution_script.encode(input)
	return [got, want]


func test_arbitrary_quadruple_byte(solution_script):
	var want = [192, 128, 128, 0]

	var input = [134217728]
	var got = solution_script.encode(input)
	return [got, want]


func test_asymmetric_quadruple_byte(solution_script):
	var want = [129, 213, 238, 4]

	var input = [3503876]
	var got = solution_script.encode(input)
	return [got, want]


func test_largest_quadruple_byte(solution_script):
	var want = [255, 255, 255, 127]

	var input = [268435455]
	var got = solution_script.encode(input)
	return [got, want]


func test_smallest_quintuple_byte(solution_script):
	var want = [129, 128, 128, 128, 0]

	var input = [268435456]
	var got = solution_script.encode(input)
	return [got, want]


func test_arbitrary_quintuple_byte(solution_script):
	var want = [143, 248, 128, 128, 0]

	var input = [4278190080]
	var got = solution_script.encode(input)
	return [got, want]


func test_asymmetric_quintuple_byte(solution_script):
	var want = [136, 179, 149, 194, 5]

	var input = [2254790917]
	var got = solution_script.encode(input)
	return [got, want]


func test_maximum_32_bit_integer_input(solution_script):
	var want = [143, 255, 255, 255, 127]

	var input = [4294967295]
	var got = solution_script.encode(input)
	return [got, want]


func test_two_single_byte_values(solution_script):
	var want = [64, 127]

	var input = [64, 127]
	var got = solution_script.encode(input)
	return [got, want]


func test_two_multi_byte_values(solution_script):
	var want = [129, 128, 0, 200, 232, 86]

	var input = [16384, 1193046]
	var got = solution_script.encode(input)
	return [got, want]


func test_many_multi_byte_values(solution_script):
	var want = [192, 0, 200, 232, 86, 255, 255, 255, 127, 0, 255, 127, 129, 128, 0]

	var input = [8192, 1193046, 268435455, 0, 16383, 16384]
	var got = solution_script.encode(input)
	return [got, want]


func test_one_byte(solution_script):
	var want = [127]

	var input = [127]
	var got = solution_script.decode(input)
	return [got, want]


func test_two_bytes(solution_script):
	var want = [8192]

	var input = [192, 0]
	var got = solution_script.decode(input)
	return [got, want]


func test_three_bytes(solution_script):
	var want = [2097151]

	var input = [255, 255, 127]
	var got = solution_script.decode(input)
	return [got, want]


func test_four_bytes(solution_script):
	var want = [2097152]

	var input = [129, 128, 128, 0]
	var got = solution_script.decode(input)
	return [got, want]


func test_maximum_32_bit_integer(solution_script):
	var want = [4294967295]

	var input = [143, 255, 255, 255, 127]
	var got = solution_script.decode(input)
	return [got, want]


func test_incomplete_sequence_causes_error(solution_script):
	var want = null

	var input = [255]
	var got = solution_script.decode(input)
	return [got, want]


func test_incomplete_sequence_causes_error_even_if_value_is_zero(solution_script):
	var want = null

	var input = [128]
	var got = solution_script.decode(input)
	return [got, want]


func test_multiple_values(solution_script):
	var want = [8192, 1193046, 268435455, 0, 16383, 16384]

	var input = [192, 0, 200, 232, 86, 255, 255, 255, 127, 0, 255, 127, 129, 128, 0]
	var got = solution_script.decode(input)
	return [got, want]
