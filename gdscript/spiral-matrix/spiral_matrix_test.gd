func test_empty_spiral(solution_script):
	return [solution_script.spiral_matrix(0), []]


func test_trivial_spiral(solution_script):
	return [solution_script.spiral_matrix(1), [[1]]]


func test_spiral_of_size_2(solution_script):
	return [
		solution_script.spiral_matrix(2),
		[
			[1, 2],
			[4, 3]
		]
	]


func test_spiral_of_size_3(solution_script):
	return [
		solution_script.spiral_matrix(3),
		[
			[1, 2, 3],
			[8, 9, 4],
			[7, 6, 5]
		]
	]


func test_spiral_of_size_4(solution_script):
	return [
		solution_script.spiral_matrix(4),
		[
			[1, 2, 3, 4],
			[12, 13, 14, 5],
			[11, 16, 15, 6],
			[10, 9, 8, 7]
		]
	]


func test_spiral_of_size_5(solution_script):
	return [
		solution_script.spiral_matrix(5),
		[
			[1, 2, 3, 4, 5],
			[16, 17, 18, 19, 6],
			[15, 24, 25, 20, 7],
			[14, 23, 22, 21, 8],
			[13, 12, 11, 10, 9],
		]
	]
