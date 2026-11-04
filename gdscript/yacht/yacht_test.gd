func test_yacht(solution_script):
	var got = solution_script.score([5, 5, 5, 5, 5], solution_script.YACHT)
	var want = 50
	return [got, want]


func test_not_yacht(solution_script):
	var got = solution_script.score([1, 3, 3, 2, 5], solution_script.YACHT)
	var want = 0
	return [got, want]


func test_ones(solution_script):
	var got = solution_script.score([1, 1, 1, 3, 5], solution_script.ONES)
	var want = 3
	return [got, want]


func test_ones_out_of_order(solution_script):
	var got = solution_script.score([3, 1, 1, 5, 1], solution_script.ONES)
	var want = 3
	return [got, want]


func test_no_ones(solution_script):
	var got = solution_script.score([4, 3, 6, 5, 5], solution_script.ONES)
	var want = 0
	return [got, want]


func test_twos(solution_script):
	var got = solution_script.score([2, 3, 4, 5, 6], solution_script.TWOS)
	var want = 2
	return [got, want]


func test_fours(solution_script):
	var got = solution_script.score([1, 4, 1, 4, 1], solution_script.FOURS)
	var want = 8
	return [got, want]


func test_yacht_counted_as_threes(solution_script):
	var got = solution_script.score([3, 3, 3, 3, 3], solution_script.THREES)
	var want = 15
	return [got, want]


func test_yacht_of_3s_counted_as_fives(solution_script):
	var got = solution_script.score([3, 3, 3, 3, 3], solution_script.FIVES)
	var want = 0
	return [got, want]


func test_fives(solution_script):
	var got = solution_script.score([1, 5, 3, 5, 3], solution_script.FIVES)
	var want = 10
	return [got, want]


func test_sixes(solution_script):
	var got = solution_script.score([2, 3, 4, 5, 6], solution_script.SIXES)
	var want = 6
	return [got, want]


func test_full_house_two_small_three_big(solution_script):
	var got = solution_script.score([2, 2, 4, 4, 4], solution_script.FULL_HOUSE)
	var want = 16
	return [got, want]


func test_full_house_three_small_two_big(solution_script):
	var got = solution_script.score([5, 3, 3, 5, 3], solution_script.FULL_HOUSE)
	var want = 19
	return [got, want]


func test_two_pair_is_not_a_full_house(solution_script):
	var got = solution_script.score([2, 2, 4, 4, 5], solution_script.FULL_HOUSE)
	var want = 0
	return [got, want]


func test_four_of_a_kind_is_not_a_full_house(solution_script):
	var got = solution_script.score([1, 4, 4, 4, 4], solution_script.FULL_HOUSE)
	var want = 0
	return [got, want]


func test_yacht_is_not_a_full_house(solution_script):
	var got = solution_script.score([2, 2, 2, 2, 2], solution_script.FULL_HOUSE)
	var want = 0
	return [got, want]


func test_four_of_a_kind(solution_script):
	var got = solution_script.score([6, 6, 4, 6, 6], solution_script.FOUR_OF_A_KIND)
	var want = 24
	return [got, want]


func test_yacht_can_be_scored_as_four_of_a_kind(solution_script):
	var got = solution_script.score([3, 3, 3, 3, 3], solution_script.FOUR_OF_A_KIND)
	var want = 12
	return [got, want]


func test_full_house_is_not_four_of_a_kind(solution_script):
	var got = solution_script.score([3, 3, 3, 5, 5], solution_script.FOUR_OF_A_KIND)
	var want = 0
	return [got, want]


func test_little_straight(solution_script):
	var got = solution_script.score([3, 5, 4, 1, 2], solution_script.LITTLE_STRAIGHT)
	var want = 30
	return [got, want]


func test_little_straight_as_big_straight(solution_script):
	var got = solution_script.score([1, 2, 3, 4, 5], solution_script.BIG_STRAIGHT)
	var want = 0
	return [got, want]


func test_four_in_order_but_not_a_little_straight(solution_script):
	var got = solution_script.score([1, 1, 2, 3, 4], solution_script.LITTLE_STRAIGHT)
	var want = 0
	return [got, want]


func test_no_pairs_but_not_a_little_straight(solution_script):
	var got = solution_script.score([1, 2, 3, 4, 6], solution_script.LITTLE_STRAIGHT)
	var want = 0
	return [got, want]


func test_minimum_is_1_maximum_is_5_but_not_a_little_straight(solution_script):
	var got = solution_script.score([1, 1, 3, 4, 5], solution_script.LITTLE_STRAIGHT)
	var want = 0
	return [got, want]


func test_big_straight(solution_script):
	var got = solution_script.score([4, 6, 2, 5, 3], solution_script.BIG_STRAIGHT)
	var want = 30
	return [got, want]


func test_big_straight_as_little_straight(solution_script):
	var got = solution_script.score([6, 5, 4, 3, 2], solution_script.LITTLE_STRAIGHT)
	var want = 0
	return [got, want]


func test_no_pairs_but_not_a_big_straight(solution_script):
	var got = solution_script.score([6, 5, 4, 3, 1], solution_script.BIG_STRAIGHT)
	var want = 0
	return [got, want]


func test_choice(solution_script):
	var got = solution_script.score([3, 3, 5, 6, 6], solution_script.CHOICE)
	var want = 23
	return [got, want]


func test_yacht_as_choice(solution_script):
	var got = solution_script.score([2, 2, 2, 2, 2], solution_script.CHOICE)
	var want = 10
	return [got, want]
