func test_empty_list_has_length_of_zero(solution_script):
	var linked_list = solution_script.LinkedList.new([])
	var res = 0

	res = linked_list.count()
	if res != 0:
		return [false, "count() = %d; expected %d" % [res, 0]]

	return [true, true]


func test_singleton_list_has_length_of_one(solution_script):
	var linked_list = solution_script.LinkedList.new([1])
	var res = 0

	res = linked_list.count()
	if res != 1:
		return [false, "count() = %d; expected %d" % [res, 1]]

	return [true, true]


func test_non_empty_list_has_correct_length(solution_script):
	var linked_list = solution_script.LinkedList.new([1, 2, 3])
	var res = 0

	res = linked_list.count()
	if res != 3:
		return [false, "count() = %d; expected %d" % [res, 3]]

	return [true, true]


func test_pop_from_empty_list_is_an_error(solution_script):
	var linked_list = solution_script.LinkedList.new([])
	var res = 0

	res = linked_list.pop()
	if res != null:
		return [false, "pop() = %d; expected null" % res]

	return [true, true]


func test_can_pop_from_singleton_list(solution_script):
	var linked_list = solution_script.LinkedList.new([1])
	var res = 0

	res = linked_list.pop()
	if res != 1:
		return [false, "pop() = %d; expected %d" % [res, 1]]

	return [true, true]


func test_can_pop_from_non_empty_list(solution_script):
	var linked_list = solution_script.LinkedList.new([1, 2])
	var res = 0

	res = linked_list.pop()
	if res != 2:
		return [false, "pop() = %d; expected %d" % [res, 2]]

	return [true, true]


func test_can_pop_multiple_items(solution_script):
	var linked_list = solution_script.LinkedList.new([1, 2])
	var res = 0

	res = linked_list.pop()
	if res != 2:
		return [false, "pop() = %d; expected %d" % [res, 2]]

	res = linked_list.pop()
	if res != 1:
		return [false, "pop() = %d; expected %d" % [res, 1]]

	return [true, true]


func test_pop_updates_the_count(solution_script):
	var linked_list = solution_script.LinkedList.new([1, 2])
	var res = 0

	res = linked_list.count()
	if res != 2:
		return [false, "count() = %d; expected %d" % [res, 2]]

	res = linked_list.pop()
	if res != 2:
		return [false, "pop() = %d; expected %d" % [res, 2]]

	res = linked_list.count()
	if res != 1:
		return [false, "count() = %d; expected %d" % [res, 1]]

	res = linked_list.pop()
	if res != 1:
		return [false, "pop() = %d; expected %d" % [res, 1]]

	res = linked_list.count()
	if res != 0:
		return [false, "count() = %d; expected %d" % [res, 0]]

	return [true, true]


func test_can_push_to_an_empty_list(solution_script):
	var linked_list = solution_script.LinkedList.new([])
	var res = 0

	linked_list.push(1)

	return [true, true]


func test_can_push_to_a_non_empty_list(solution_script):
	var linked_list = solution_script.LinkedList.new([1, 2])
	var res = 0

	linked_list.push(3)

	return [true, true]


func test_push_updates_count(solution_script):
	var linked_list = solution_script.LinkedList.new([1, 2])
	var res = 0

	linked_list.push(3)

	res = linked_list.count()
	if res != 3:
		return [false, "count() = %d; expected %d" % [res, 3]]

	return [true, true]


func test_push_and_pop(solution_script):
	var linked_list = solution_script.LinkedList.new([])
	var res = 0

	linked_list.push(1)

	linked_list.push(2)

	res = linked_list.pop()
	if res != 2:
		return [false, "pop() = %d; expected %d" % [res, 2]]

	linked_list.push(3)

	res = linked_list.count()
	if res != 2:
		return [false, "count() = %d; expected %d" % [res, 2]]

	res = linked_list.pop()
	if res != 3:
		return [false, "pop() = %d; expected %d" % [res, 3]]

	res = linked_list.pop()
	if res != 1:
		return [false, "pop() = %d; expected %d" % [res, 1]]

	res = linked_list.count()
	if res != 0:
		return [false, "count() = %d; expected %d" % [res, 0]]

	return [true, true]


func test_peek_on_empty_list_is_an_error(solution_script):
	var linked_list = solution_script.LinkedList.new([])
	var res = 0

	res = linked_list.peek()
	if res != null:
		return [false, "peek() = %d, expected null" % res]

	return [true, true]


func test_can_peek_on_singleton_list(solution_script):
	var linked_list = solution_script.LinkedList.new([1])
	var res = 0

	res = linked_list.peek()
	if res != 1:
		return [false, "peek() = %d; expected %d" % [res, 1]]

	return [true, true]


func test_can_peek_on_non_empty_list(solution_script):
	var linked_list = solution_script.LinkedList.new([1, 2])
	var res = 0

	res = linked_list.peek()
	if res != 2:
		return [false, "peek() = %d; expected %d" % [res, 2]]

	return [true, true]


func test_peek_does_not_change_the_count(solution_script):
	var linked_list = solution_script.LinkedList.new([1, 2])
	var res = 0

	res = linked_list.peek()
	if res != 2:
		return [false, "peek() = %d; expected %d" % [res, 2]]

	res = linked_list.count()
	if res != 2:
		return [false, "count() = %d; expected %d" % [res, 2]]

	return [true, true]


func test_can_peek_after_a_pop_and_push(solution_script):
	var linked_list = solution_script.LinkedList.new([])
	var res = 0

	linked_list.push(1)

	linked_list.push(2)

	res = linked_list.peek()
	if res != 2:
		return [false, "peek() = %d; expected %d" % [res, 2]]

	res = linked_list.pop()
	if res != 2:
		return [false, "pop() = %d; expected %d" % [res, 2]]

	res = linked_list.peek()
	if res != 1:
		return [false, "peek() = %d; expected %d" % [res, 1]]

	linked_list.push(3)

	res = linked_list.peek()
	if res != 3:
		return [false, "peek() = %d; expected %d" % [res, 3]]

	return [true, true]


func test_empty_linked_list_to_list_is_empty(solution_script):
	var linked_list = solution_script.LinkedList.new([])
	var res = 0

	res = linked_list.to_list()
	if res != []:
		return [false, "to_list() = %s; expected %s" % [res, []]]

	return [true, true]


func test_to_list_with_multiple_values(solution_script):
	var linked_list = solution_script.LinkedList.new([1, 2, 3])
	var res = 0

	res = linked_list.to_list()
	if res != [1, 2, 3]:
		return [false, "to_list() = %s; expected %s" % [res, [1, 2, 3]]]

	return [true, true]


func test_to_list_after_a_pop(solution_script):
	var linked_list = solution_script.LinkedList.new([])
	var res = 0

	linked_list.push(1)

	linked_list.push(2)

	linked_list.push(3)

	res = linked_list.pop()
	if res != 3:
		return [false, "pop() = %d; expected %d" % [res, 3]]

	linked_list.push(4)

	res = linked_list.to_list()
	if res != [1, 2, 4]:
		return [false, "to_list() = %s; expected %s" % [res, [1, 2, 4]]]

	return [true, true]


func test_reversed_empty_list_has_same_values(solution_script):
	var linked_list = solution_script.LinkedList.new([])
	var res = 0

	linked_list.reverse()

	res = linked_list.to_list()
	if res != []:
		return [false, "to_list() = %s; expected %s" % [res, []]]

	return [true, true]


func test_reversed_singleton_list_is_same_list(solution_script):
	var linked_list = solution_script.LinkedList.new([1])
	var res = 0

	linked_list.reverse()

	res = linked_list.to_list()
	if res != [1]:
		return [false, "to_list() = %s; expected %s" % [res, [1]]]

	return [true, true]


func test_reversed_non_empty_list_is_reversed(solution_script):
	var linked_list = solution_script.LinkedList.new([1, 2, 3])
	var res = 0

	linked_list.reverse()

	res = linked_list.count()
	if res != 3:
		return [false, "count() = %d; expected %d" % [res, 3]]

	res = linked_list.pop()
	if res != 1:
		return [false, "pop() = %d; expected %d" % [res, 1]]

	res = linked_list.pop()
	if res != 2:
		return [false, "pop() = %d; expected %d" % [res, 2]]

	res = linked_list.pop()
	if res != 3:
		return [false, "pop() = %d; expected %d" % [res, 3]]

	return [true, true]


func test_double_reverse(solution_script):
	var linked_list = solution_script.LinkedList.new([1, 2, 3])
	var res = 0

	linked_list.reverse()

	linked_list.reverse()

	res = linked_list.pop()
	if res != 3:
		return [false, "pop() = %d; expected %d" % [res, 3]]

	res = linked_list.pop()
	if res != 2:
		return [false, "pop() = %d; expected %d" % [res, 2]]

	res = linked_list.pop()
	if res != 1:
		return [false, "pop() = %d; expected %d" % [res, 1]]

	return [true, true]
