func test_grains_on_square_1(solution_script):
	var got = solution_script.square(1)
	var want = 1
	return [got, want]


func test_grains_on_square_2(solution_script):
	var got = solution_script.square(2)
	var want = 2
	return [got, want]


func test_grains_on_square_3(solution_script):
	var got = solution_script.square(3)
	var want = 4
	return [got, want]


func test_grains_on_square_4(solution_script):
	var got = solution_script.square(4)
	var want = 8
	return [got, want]


func test_grains_on_square_16(solution_script):
	var got = solution_script.square(16)
	var want = 32768
	return [got, want]


func test_grains_on_square_32(solution_script):
	var got = solution_script.square(32)
	var want = 2147483648
	return [got, want]


func test_square_0_is_invalid(solution_script):
	var got = solution_script.square(0)
	var want = null
	return [got, want]


func test_negative_square_is_invalid(solution_script):
	var got = solution_script.square(-1)
	var want = null
	return [got, want]

func test_grains_on_square_63(solution_script):
	var got = solution_script.square(63)
	var want = 4611686018427387904
	return [got, want]


func test_square_greater_than_63_is_invalid(solution_script):
	var got = solution_script.square(64)
	var want = null
	return [got, want]


func test_returns_the_total_number_of_grains_on_the_board(solution_script):
	var got = solution_script.total()
	var want = 9223372036854775807
	return [got, want]
