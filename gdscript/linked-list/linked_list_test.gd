func test_pop_gets_element_from_the_list(solution_script):
	var linked_list = solution_script.LinkedList.new()
	var res = 0

	linked_list.push(7)

	res = linked_list.pop()
	if res != 7:
		return [false, "pop() = %d; expected %d" % [res, 7]]

	return [true, true]


func test_push_pop_respectively_add_remove_at_the_end_of_the_list(solution_script):
	var linked_list = solution_script.LinkedList.new()
	var res = 0

	linked_list.push(11)

	linked_list.push(13)

	res = linked_list.pop()
	if res != 13:
		return [false, "pop() = %d; expected %d" % [res, 13]]

	res = linked_list.pop()
	if res != 11:
		return [false, "pop() = %d; expected %d" % [res, 11]]

	return [true, true]


func test_shift_gets_an_element_from_the_list(solution_script):
	var linked_list = solution_script.LinkedList.new()
	var res = 0

	linked_list.push(17)

	res = linked_list.shift()
	if res != 17:
		return [false, "shift() = %d; expected %d" % [res, 17]]

	return [true, true]


func test_shift_gets_first_element_from_the_list(solution_script):
	var linked_list = solution_script.LinkedList.new()
	var res = 0

	linked_list.push(23)

	linked_list.push(5)

	res = linked_list.shift()
	if res != 23:
		return [false, "shift() = %d; expected %d" % [res, 23]]

	res = linked_list.shift()
	if res != 5:
		return [false, "shift() = %d; expected %d" % [res, 5]]

	return [true, true]


func test_unshift_adds_element_at_start_of_the_list(solution_script):
	var linked_list = solution_script.LinkedList.new()
	var res = 0

	linked_list.unshift(23)

	linked_list.unshift(5)

	res = linked_list.shift()
	if res != 5:
		return [false, "shift() = %d; expected %d" % [res, 5]]

	res = linked_list.shift()
	if res != 23:
		return [false, "shift() = %d; expected %d" % [res, 23]]

	return [true, true]


func test_pop_push_shift_and_unshift_can_be_used_in_any_order(solution_script):
	var linked_list = solution_script.LinkedList.new()
	var res = 0

	linked_list.push(1)

	linked_list.push(2)

	res = linked_list.pop()
	if res != 2:
		return [false, "pop() = %d; expected %d" % [res, 2]]

	linked_list.push(3)

	res = linked_list.shift()
	if res != 1:
		return [false, "shift() = %d; expected %d" % [res, 1]]

	linked_list.unshift(4)

	linked_list.push(5)

	res = linked_list.shift()
	if res != 4:
		return [false, "shift() = %d; expected %d" % [res, 4]]

	res = linked_list.pop()
	if res != 5:
		return [false, "pop() = %d; expected %d" % [res, 5]]

	res = linked_list.shift()
	if res != 3:
		return [false, "shift() = %d; expected %d" % [res, 3]]

	return [true, true]


func test_count_an_empty_list(solution_script):
	var linked_list = solution_script.LinkedList.new()
	var res = 0

	res = linked_list.count()
	if res != 0:
		return [false, "count() = %d; expected %d" % [res, 0]]

	return [true, true]


func test_count_a_list_with_items(solution_script):
	var linked_list = solution_script.LinkedList.new()
	var res = 0

	linked_list.push(37)

	linked_list.push(1)

	res = linked_list.count()
	if res != 2:
		return [false, "count() = %d; expected %d" % [res, 2]]

	return [true, true]


func test_count_is_correct_after_mutation(solution_script):
	var linked_list = solution_script.LinkedList.new()
	var res = 0

	linked_list.push(31)

	res = linked_list.count()
	if res != 1:
		return [false, "count() = %d; expected %d" % [res, 1]]

	linked_list.unshift(43)

	res = linked_list.count()
	if res != 2:
		return [false, "count() = %d; expected %d" % [res, 2]]

	linked_list.shift()

	res = linked_list.count()
	if res != 1:
		return [false, "count() = %d; expected %d" % [res, 1]]

	linked_list.pop()

	res = linked_list.count()
	if res != 0:
		return [false, "count() = %d; expected %d" % [res, 0]]

	return [true, true]


func test_popping_to_empty_doesn_t_break_the_list(solution_script):
	var linked_list = solution_script.LinkedList.new()
	var res = 0

	linked_list.push(41)

	linked_list.push(59)

	linked_list.pop()

	linked_list.pop()

	linked_list.push(47)

	res = linked_list.count()
	if res != 1:
		return [false, "count() = %d; expected %d" % [res, 1]]

	res = linked_list.pop()
	if res != 47:
		return [false, "pop() = %d; expected %d" % [res, 47]]

	return [true, true]


func test_shifting_to_empty_doesn_t_break_the_list(solution_script):
	var linked_list = solution_script.LinkedList.new()
	var res = 0

	linked_list.push(41)

	linked_list.push(59)

	linked_list.shift()

	linked_list.shift()

	linked_list.push(47)

	res = linked_list.count()
	if res != 1:
		return [false, "count() = %d; expected %d" % [res, 1]]

	res = linked_list.shift()
	if res != 47:
		return [false, "shift() = %d; expected %d" % [res, 47]]

	return [true, true]


func test_deletes_the_only_element(solution_script):
	var linked_list = solution_script.LinkedList.new()
	var res = 0

	linked_list.push(61)

	linked_list.delete(61)

	res = linked_list.count()
	if res != 0:
		return [false, "count() = %d; expected %d" % [res, 0]]

	return [true, true]


func test_deletes_the_element_with_the_specified_value_from_the_list(solution_script):
	var linked_list = solution_script.LinkedList.new()
	var res = 0

	linked_list.push(71)

	linked_list.push(83)

	linked_list.push(79)

	linked_list.delete(83)

	res = linked_list.count()
	if res != 2:
		return [false, "count() = %d; expected %d" % [res, 2]]

	res = linked_list.pop()
	if res != 79:
		return [false, "pop() = %d; expected %d" % [res, 79]]

	res = linked_list.shift()
	if res != 71:
		return [false, "shift() = %d; expected %d" % [res, 71]]

	return [true, true]


func test_deletes_the_element_with_the_specified_value_from_the_list_re_assigns_tail(solution_script):
	var linked_list = solution_script.LinkedList.new()
	var res = 0

	linked_list.push(71)

	linked_list.push(83)

	linked_list.push(79)

	linked_list.delete(83)

	res = linked_list.count()
	if res != 2:
		return [false, "count() = %d; expected %d" % [res, 2]]

	res = linked_list.pop()
	if res != 79:
		return [false, "pop() = %d; expected %d" % [res, 79]]

	res = linked_list.pop()
	if res != 71:
		return [false, "pop() = %d; expected %d" % [res, 71]]

	return [true, true]


func test_deletes_the_element_with_the_specified_value_from_the_list_re_assigns_head(solution_script):
	var linked_list = solution_script.LinkedList.new()
	var res = 0

	linked_list.push(71)

	linked_list.push(83)

	linked_list.push(79)

	linked_list.delete(83)

	res = linked_list.count()
	if res != 2:
		return [false, "count() = %d; expected %d" % [res, 2]]

	res = linked_list.shift()
	if res != 71:
		return [false, "shift() = %d; expected %d" % [res, 71]]

	res = linked_list.shift()
	if res != 79:
		return [false, "shift() = %d; expected %d" % [res, 79]]

	return [true, true]


func test_deletes_the_first_of_two_elements(solution_script):
	var linked_list = solution_script.LinkedList.new()
	var res = 0

	linked_list.push(97)

	linked_list.push(101)

	linked_list.delete(97)

	res = linked_list.count()
	if res != 1:
		return [false, "count() = %d; expected %d" % [res, 1]]

	res = linked_list.pop()
	if res != 101:
		return [false, "pop() = %d; expected %d" % [res, 101]]

	return [true, true]


func test_deletes_the_second_of_two_elements(solution_script):
	var linked_list = solution_script.LinkedList.new()
	var res = 0

	linked_list.push(97)

	linked_list.push(101)

	linked_list.delete(101)

	res = linked_list.count()
	if res != 1:
		return [false, "count() = %d; expected %d" % [res, 1]]

	res = linked_list.pop()
	if res != 97:
		return [false, "pop() = %d; expected %d" % [res, 97]]

	return [true, true]


func test_delete_does_not_modify_the_list_if_the_element_is_not_found(solution_script):
	var linked_list = solution_script.LinkedList.new()
	var res = 0

	linked_list.push(89)

	linked_list.delete(103)

	res = linked_list.count()
	if res != 1:
		return [false, "count() = %d; expected %d" % [res, 1]]

	return [true, true]


func test_deletes_only_the_first_occurrence(solution_script):
	var linked_list = solution_script.LinkedList.new()
	var res = 0

	linked_list.push(73)

	linked_list.push(9)

	linked_list.push(9)

	linked_list.push(107)

	linked_list.delete(9)

	res = linked_list.count()
	if res != 3:
		return [false, "count() = %d; expected %d" % [res, 3]]

	res = linked_list.pop()
	if res != 107:
		return [false, "pop() = %d; expected %d" % [res, 107]]

	res = linked_list.pop()
	if res != 9:
		return [false, "pop() = %d; expected %d" % [res, 9]]

	res = linked_list.pop()
	if res != 73:
		return [false, "pop() = %d; expected %d" % [res, 73]]

	return [true, true]
