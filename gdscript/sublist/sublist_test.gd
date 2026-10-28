func test_empty_lists(solution_script):
	var want = "equal"
	var list_one = []
	var list_two = []
	var got = solution_script.sublist(list_one, list_two)
	return [got, want]


func test_empty_list_within_non_empty_list(solution_script):
	var want = "sublist"
	var list_one = []
	var list_two = [1, 2, 3]
	var got = solution_script.sublist(list_one, list_two)
	return [got, want]


func test_non_empty_list_contains_empty_list(solution_script):
	var want = "superlist"
	var list_one = [1, 2, 3]
	var list_two = []
	var got = solution_script.sublist(list_one, list_two)
	return [got, want]


func test_list_equals_itself(solution_script):
	var want = "equal"
	var list_one = [1, 2, 3]
	var list_two = [1, 2, 3]
	var got = solution_script.sublist(list_one, list_two)
	return [got, want]


func test_different_lists(solution_script):
	var want = "unequal"
	var list_one = [1, 2, 3]
	var list_two = [2, 3, 4]
	var got = solution_script.sublist(list_one, list_two)
	return [got, want]


func test_false_start(solution_script):
	var want = "sublist"
	var list_one = [1, 2, 5]
	var list_two = [0, 1, 2, 3, 1, 2, 5, 6]
	var got = solution_script.sublist(list_one, list_two)
	return [got, want]


func test_consecutive(solution_script):
	var want = "sublist"
	var list_one = [1, 1, 2]
	var list_two = [0, 1, 1, 1, 2, 1, 2]
	var got = solution_script.sublist(list_one, list_two)
	return [got, want]


func test_sublist_at_start(solution_script):
	var want = "sublist"
	var list_one = [0, 1, 2]
	var list_two = [0, 1, 2, 3, 4, 5]
	var got = solution_script.sublist(list_one, list_two)
	return [got, want]


func test_sublist_in_middle(solution_script):
	var want = "sublist"
	var list_one = [2, 3, 4]
	var list_two = [0, 1, 2, 3, 4, 5]
	var got = solution_script.sublist(list_one, list_two)
	return [got, want]


func test_sublist_at_end(solution_script):
	var want = "sublist"
	var list_one = [3, 4, 5]
	var list_two = [0, 1, 2, 3, 4, 5]
	var got = solution_script.sublist(list_one, list_two)
	return [got, want]


func test_at_start_of_superlist(solution_script):
	var want = "superlist"
	var list_one = [0, 1, 2, 3, 4, 5]
	var list_two = [0, 1, 2]
	var got = solution_script.sublist(list_one, list_two)
	return [got, want]


func test_in_middle_of_superlist(solution_script):
	var want = "superlist"
	var list_one = [0, 1, 2, 3, 4, 5]
	var list_two = [2, 3]
	var got = solution_script.sublist(list_one, list_two)
	return [got, want]


func test_at_end_of_superlist(solution_script):
	var want = "superlist"
	var list_one = [0, 1, 2, 3, 4, 5]
	var list_two = [3, 4, 5]
	var got = solution_script.sublist(list_one, list_two)
	return [got, want]


func test_first_list_missing_element_from_second_list(solution_script):
	var want = "unequal"
	var list_one = [1, 3]
	var list_two = [1, 2, 3]
	var got = solution_script.sublist(list_one, list_two)
	return [got, want]


func test_second_list_missing_element_from_first_list(solution_script):
	var want = "unequal"
	var list_one = [1, 2, 3]
	var list_two = [1, 3]
	var got = solution_script.sublist(list_one, list_two)
	return [got, want]


func test_first_list_missing_additional_digits_from_second_list(solution_script):
	var want = "unequal"
	var list_one = [1, 2]
	var list_two = [1, 22]
	var got = solution_script.sublist(list_one, list_two)
	return [got, want]


func test_order_matters_to_a_list(solution_script):
	var want = "unequal"
	var list_one = [1, 2, 3]
	var list_two = [3, 2, 1]
	var got = solution_script.sublist(list_one, list_two)
	return [got, want]


func test_same_digits_but_different_numbers(solution_script):
	var want = "unequal"
	var list_one = [1, 0, 1]
	var list_two = [10, 1]
	var got = solution_script.sublist(list_one, list_two)
	return [got, want]
