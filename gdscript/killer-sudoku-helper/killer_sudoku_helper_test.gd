func test_1(solution_script):
	var got = solution_script.combinations(1, 1, [])
	var want = [[1]]
	got.sort()
	want.sort()
	return [got, want]


func test_2(solution_script):
	var got = solution_script.combinations(2, 1, [])
	var want = [[2]]
	got.sort()
	want.sort()
	return [got, want]


func test_3(solution_script):
	var got = solution_script.combinations(3, 1, [])
	var want = [[3]]
	got.sort()
	want.sort()
	return [got, want]


func test_4(solution_script):
	var got = solution_script.combinations(4, 1, [])
	var want = [[4]]
	got.sort()
	want.sort()
	return [got, want]


func test_5(solution_script):
	var got = solution_script.combinations(5, 1, [])
	var want = [[5]]
	got.sort()
	want.sort()
	return [got, want]


func test_6(solution_script):
	var got = solution_script.combinations(6, 1, [])
	var want = [[6]]
	got.sort()
	want.sort()
	return [got, want]


func test_7(solution_script):
	var got = solution_script.combinations(7, 1, [])
	var want = [[7]]
	got.sort()
	want.sort()
	return [got, want]


func test_8(solution_script):
	var got = solution_script.combinations(8, 1, [])
	var want = [[8]]
	got.sort()
	want.sort()
	return [got, want]


func test_9(solution_script):
	var got = solution_script.combinations(9, 1, [])
	var want = [[9]]
	got.sort()
	want.sort()
	return [got, want]


func test_cage_with_sum_45_contains_all_digits_1_9(solution_script):
	var got = solution_script.combinations(45, 9, [])
	var want = [[1, 2, 3, 4, 5, 6, 7, 8, 9]]
	got.sort()
	want.sort()
	return [got, want]


func test_cage_with_only_1_possible_combination(solution_script):
	var got = solution_script.combinations(7, 3, [])
	var want = [[1, 2, 4]]
	got.sort()
	want.sort()
	return [got, want]


func test_cage_with_several_combinations(solution_script):
	var got = solution_script.combinations(10, 2, [])
	var want = [[1, 9], [2, 8], [3, 7], [4, 6]]
	got.sort()
	want.sort()
	return [got, want]


func test_cage_with_several_combinations_that_is_restricted(solution_script):
	var got = solution_script.combinations(10, 2, [1, 4])
	var want = [[2, 8], [3, 7]]
	got.sort()
	want.sort()
	return [got, want]
