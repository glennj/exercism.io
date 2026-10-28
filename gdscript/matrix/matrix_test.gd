func test_extract_row_from_one_number_matrix(solution_script):
	var matrix = solution_script.Matrix.new("1")
	var got = matrix.row(1)
	var want = [1]
	return [got, want]


func test_can_extract_row(solution_script):
	var matrix = solution_script.Matrix.new("1 2\n3 4")
	var got = matrix.row(2)
	var want = [3, 4]
	return [got, want]


func test_extract_row_where_numbers_have_different_widths(solution_script):
	var matrix = solution_script.Matrix.new("1 2\n10 20")
	var got = matrix.row(2)
	var want = [10, 20]
	return [got, want]


func test_can_extract_row_from_non_square_matrix_with_no_corresponding_column(solution_script):
	var matrix = solution_script.Matrix.new("1 2 3\n4 5 6\n7 8 9\n8 7 6")
	var got = matrix.row(4)
	var want = [8, 7, 6]
	return [got, want]


func test_extract_column_from_one_number_matrix(solution_script):
	var matrix = solution_script.Matrix.new("1")
	var got = matrix.column(1)
	var want = [1]
	return [got, want]


func test_can_extract_column(solution_script):
	var matrix = solution_script.Matrix.new("1 2 3\n4 5 6\n7 8 9")
	var got = matrix.column(3)
	var want = [3, 6, 9]
	return [got, want]


func test_can_extract_column_from_non_square_matrix_with_no_corresponding_row(solution_script):
	var matrix = solution_script.Matrix.new("1 2 3 4\n5 6 7 8\n9 8 7 6")
	var got = matrix.column(4)
	var want = [4, 8, 6]
	return [got, want]


func test_extract_column_where_numbers_have_different_widths(solution_script):
	var matrix = solution_script.Matrix.new("89 1903 3\n18 3 1\n9 4 800")
	var got = matrix.column(2)
	var want = [1903, 3, 4]
	return [got, want]
