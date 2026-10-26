func test_date_only_specification_of_time(solution_script):
	var got = solution_script.add("2011-04-25")
	var want = "2043-01-01T01:46:40"
	return [got, want]


func test_second_test_for_date_only_specification_of_time(solution_script):
	var got = solution_script.add("1977-06-13")
	var want = "2009-02-19T01:46:40"
	return [got, want]


func test_third_test_for_date_only_specification_of_time(solution_script):
	var got = solution_script.add("1959-07-19")
	var want = "1991-03-27T01:46:40"
	return [got, want]


func test_full_time_specified(solution_script):
	var got = solution_script.add("2015-01-24T22:00:00")
	var want = "2046-10-02T23:46:40"
	return [got, want]


func test_full_time_with_day_roll_over(solution_script):
	var got = solution_script.add("2015-01-24T23:59:59")
	var want = "2046-10-03T01:46:39"
	return [got, want]
