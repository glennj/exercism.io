func test_measure_using_bucket_one_of_size_3_and_bucket_two_of_size_5_start_with_bucket_one(solution_script):
	var want = {
		"moves": 4,
		"goal_bucket": "one",
		"other_bucket": 5,
	}
	var got = solution_script.measure(3, 5, 1, "one")
	return [got, want]


func test_measure_using_bucket_one_of_size_3_and_bucket_two_of_size_5_start_with_bucket_two(solution_script):
	var want = {
		"moves": 8,
		"goal_bucket": "two",
		"other_bucket": 3,
	}
	var got = solution_script.measure(3, 5, 1, "two")
	return [got, want]


func test_measure_using_bucket_one_of_size_7_and_bucket_two_of_size_11_start_with_bucket_one(solution_script):
	var want = {
		"moves": 14,
		"goal_bucket": "one",
		"other_bucket": 11,
	}
	var got = solution_script.measure(7, 11, 2, "one")
	return [got, want]


func test_measure_using_bucket_one_of_size_7_and_bucket_two_of_size_11_start_with_bucket_two(solution_script):
	var want = {
		"moves": 18,
		"goal_bucket": "two",
		"other_bucket": 7,
	}
	var got = solution_script.measure(7, 11, 2, "two")
	return [got, want]


func test_measure_one_step_using_bucket_one_of_size_1_and_bucket_two_of_size_3_start_with_bucket_two(solution_script):
	var want = {
		"moves": 1,
		"goal_bucket": "two",
		"other_bucket": 0,
	}
	var got = solution_script.measure(1, 3, 3, "two")
	return [got, want]


func test_measure_using_bucket_one_of_size_2_and_bucket_two_of_size_3_start_with_bucket_one_and_end_with_bucket_two(solution_script):
	var want = {
		"moves": 2,
		"goal_bucket": "two",
		"other_bucket": 2,
	}
	var got = solution_script.measure(2, 3, 3, "one")
	return [got, want]


func test_measure_using_bucket_one_much_bigger_than_bucket_two(solution_script):
	var want = {
		"moves": 6,
		"goal_bucket": "one",
		"other_bucket": 1,
	}
	var got = solution_script.measure(5, 1, 2, "one")
	return [got, want]


func test_measure_using_bucket_one_much_smaller_than_bucket_two(solution_script):
	var want = {
		"moves": 6,
		"goal_bucket": "two",
		"other_bucket": 0,
	}
	var got = solution_script.measure(3, 15, 9, "one")
	return [got, want]


func test_not_possible_to_reach_the_goal(solution_script):
	var want = null
	var got = solution_script.measure(6, 15, 5, "one")
	return [got, want]


func test_with_the_same_buckets_but_a_different_goal_then_it_is_possible(solution_script):
	var want = {
		"moves": 10,
		"goal_bucket": "two",
		"other_bucket": 0,
	}
	var got = solution_script.measure(6, 15, 9, "one")
	return [got, want]


func test_goal_larger_than_both_buckets_is_impossible(solution_script):
	var want = null
	var got = solution_script.measure(5, 7, 8, "one")
	return [got, want]
