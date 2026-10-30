func test_slices_slices_of_one_from_one(solution_script):
	var expected = ['1']
	var got = solution_script.slices("1", 1)
	return [got, expected]


func test_slices_slices_of_one_from_two(solution_script):
	var expected = ['1', '2']
	var got = solution_script.slices("12", 1)
	return [got, expected]


func test_slices_slices_of_two(solution_script):
	var expected = ['35']
	var got = solution_script.slices("35", 2)
	return [got, expected]


func test_slices_slices_of_two_overlap(solution_script):
	var expected = ['91', '14', '42']
	var got = solution_script.slices("9142", 2)
	return [got, expected]


func test_slices_slices_can_include_duplicates(solution_script):
	var expected = ['777', '777', '777', '777']
	var got = solution_script.slices("777777", 3)
	return [got, expected]


func test_slices_slices_of_a_long_series(solution_script):
	var expected = ['91849', '18493', '84939', '49390', '93904', '39042', '90424', '04243']
	var got = solution_script.slices("918493904243", 5)
	return [got, expected]


func test_slices_slice_length_is_too_large(solution_script):
	var expected = null
	var got = solution_script.slices("12345", 6)
	return [got, expected]


func test_slices_slice_length_is_way_too_large(solution_script):
	var expected = null
	var got = solution_script.slices("12345", 42)
	return [got, expected]


func test_slices_slice_length_cannot_be_zero(solution_script):
	var expected = null
	var got = solution_script.slices("12345", 0)
	return [got, expected]


func test_slices_slice_length_cannot_be_negative(solution_script):
	var expected = null
	var got = solution_script.slices("123", -1)
	return [got, expected]


func test_slices_empty_series_is_invalid(solution_script):
	var expected = null
	var got = solution_script.slices("", 1)
	return [got, expected]
