func test_wink_for_1(solution_script):
	var got = solution_script.commands(1)
	var want = ["wink"]
	return [got, want]


func test_double_blink_for_10(solution_script):
	var got = solution_script.commands(2)
	var want = ["double blink"]
	return [got, want]


func test_close_your_eyes_for_100(solution_script):
	var got = solution_script.commands(4)
	var want = ["close your eyes"]
	return [got, want]


func test_jump_for_1000(solution_script):
	var got = solution_script.commands(8)
	var want = ["jump"]
	return [got, want]


func test_combine_two_actions(solution_script):
	var got = solution_script.commands(3)
	var want = ["wink", "double blink"]
	return [got, want]


func test_reverse_two_actions(solution_script):
	var got = solution_script.commands(19)
	var want = ["double blink", "wink"]
	return [got, want]


func test_reversing_one_action_gives_the_same_action(solution_script):
	var got = solution_script.commands(24)
	var want = ["jump"]
	return [got, want]


func test_reversing_no_actions_still_gives_no_actions(solution_script):
	var got = solution_script.commands(16)
	var want = []
	return [got, want]


func test_all_possible_actions(solution_script):
	var got = solution_script.commands(15)
	var want = ["wink", "double blink", "close your eyes", "jump"]
	return [got, want]


func test_reverse_all_possible_actions(solution_script):
	var got = solution_script.commands(31)
	var want = ["jump", "close your eyes", "double blink", "wink"]
	return [got, want]


func test_do_nothing_for_zero(solution_script):
	var got = solution_script.commands(0)
	var want = []
	return [got, want]
