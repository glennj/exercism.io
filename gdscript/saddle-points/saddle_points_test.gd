func test_can_identify_single_saddle_point(solution_script):
	var got = solution_script.saddle_points([[9, 8, 7], [5, 3, 2], [6, 6, 7]])
	var want = [[2, 1]]
	got.sort()
	want.sort()
	return [got, want]


func test_can_identify_that_empty_matrix_has_no_saddle_points(solution_script):
	var got = solution_script.saddle_points([[]])
	var want = []
	got.sort()
	want.sort()
	return [got, want]


func test_can_identify_lack_of_saddle_points_when_there_are_none(solution_script):
	var got = solution_script.saddle_points([[1, 2, 3], [3, 1, 2], [2, 3, 1]])
	var want = []
	got.sort()
	want.sort()
	return [got, want]


func test_can_identify_multiple_saddle_points_in_a_column(solution_script):
	var got = solution_script.saddle_points([[4, 5, 4], [3, 5, 5], [1, 5, 4]])
	var want = [[1, 2], [2, 2], [3, 2]]
	got.sort()
	want.sort()
	return [got, want]


func test_can_identify_multiple_saddle_points_in_a_row(solution_script):
	var got = solution_script.saddle_points([[6, 7, 8], [5, 5, 5], [7, 5, 6]])
	var want = [[2, 1], [2, 2], [2, 3]]
	got.sort()
	want.sort()
	return [got, want]


func test_can_identify_saddle_point_in_bottom_right_corner(solution_script):
	var got = solution_script.saddle_points([[8, 7, 9], [6, 7, 6], [3, 2, 5]])
	var want = [[3, 3]]
	got.sort()
	want.sort()
	return [got, want]


func test_can_identify_saddle_points_in_a_non_square_matrix(solution_script):
	var got = solution_script.saddle_points([[3, 1, 3], [3, 2, 4]])
	var want = [[1, 3], [1, 1]]
	got.sort()
	want.sort()
	return [got, want]


func test_can_identify_that_saddle_points_in_a_single_column_matrix_are_those_with_the_minimum_value(solution_script):
	var got = solution_script.saddle_points([[2], [1], [4], [1]])
	var want = [[2, 1], [4, 1]]
	got.sort()
	want.sort()
	return [got, want]


func test_can_identify_that_saddle_points_in_a_single_row_matrix_are_those_with_the_maximum_value(solution_script):
	var got = solution_script.saddle_points([[2, 5, 3, 5]])
	var want = [[1, 2], [1, 4]]
	got.sort()
	want.sort()
	return [got, want]
