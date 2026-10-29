func test_reading_empty_buffer_should_fail(solution_script):
	var res = null
	var buffer = solution_script.CircularBuffer.new(1)

	res = buffer.read()
	if res != null:
		return [false, "read() = %s, expected null" % res]

	return [true, true]


func test_can_read_an_item_just_written(solution_script):
	var res = null
	var buffer = solution_script.CircularBuffer.new(1)

	if not buffer.write(1):
		return [false, "write failed; expected success"]

	res = buffer.read()
	if res != 1:
		return [false, "read() = %s, expected %s" % [res, 1]]

	return [true, true]


func test_each_item_may_only_be_read_once(solution_script):
	var res = null
	var buffer = solution_script.CircularBuffer.new(1)

	if not buffer.write(1):
		return [false, "write failed; expected success"]

	res = buffer.read()
	if res != 1:
		return [false, "read() = %s, expected %s" % [res, 1]]

	res = buffer.read()
	if res != null:
		return [false, "read() = %s, expected null" % res]

	return [true, true]


func test_items_are_read_in_the_order_they_are_written(solution_script):
	var res = null
	var buffer = solution_script.CircularBuffer.new(2)

	if not buffer.write(1):
		return [false, "write failed; expected success"]

	if not buffer.write(2):
		return [false, "write failed; expected success"]

	res = buffer.read()
	if res != 1:
		return [false, "read() = %s, expected %s" % [res, 1]]

	res = buffer.read()
	if res != 2:
		return [false, "read() = %s, expected %s" % [res, 2]]

	return [true, true]


func test_full_buffer_can_t_be_written_to(solution_script):
	var res = null
	var buffer = solution_script.CircularBuffer.new(1)

	if not buffer.write(1):
		return [false, "write failed; expected success"]

	if buffer.write(2):
		return [false, "write succeeded; expected a failure"]

	return [true, true]


func test_a_read_frees_up_capacity_for_another_write(solution_script):
	var res = null
	var buffer = solution_script.CircularBuffer.new(1)

	if not buffer.write(1):
		return [false, "write failed; expected success"]

	res = buffer.read()
	if res != 1:
		return [false, "read() = %s, expected %s" % [res, 1]]

	if not buffer.write(2):
		return [false, "write failed; expected success"]

	res = buffer.read()
	if res != 2:
		return [false, "read() = %s, expected %s" % [res, 2]]

	return [true, true]


func test_read_position_is_maintained_even_across_multiple_writes(solution_script):
	var res = null
	var buffer = solution_script.CircularBuffer.new(3)

	if not buffer.write(1):
		return [false, "write failed; expected success"]

	if not buffer.write(2):
		return [false, "write failed; expected success"]

	res = buffer.read()
	if res != 1:
		return [false, "read() = %s, expected %s" % [res, 1]]

	if not buffer.write(3):
		return [false, "write failed; expected success"]

	res = buffer.read()
	if res != 2:
		return [false, "read() = %s, expected %s" % [res, 2]]

	res = buffer.read()
	if res != 3:
		return [false, "read() = %s, expected %s" % [res, 3]]

	return [true, true]


func test_items_cleared_out_of_buffer_can_t_be_read(solution_script):
	var res = null
	var buffer = solution_script.CircularBuffer.new(1)

	if not buffer.write(1):
		return [false, "write failed; expected success"]

	buffer.clear()

	res = buffer.read()
	if res != null:
		return [false, "read() = %s, expected null" % res]

	return [true, true]


func test_clear_frees_up_capacity_for_another_write(solution_script):
	var res = null
	var buffer = solution_script.CircularBuffer.new(1)

	if not buffer.write(1):
		return [false, "write failed; expected success"]

	buffer.clear()

	if not buffer.write(2):
		return [false, "write failed; expected success"]

	res = buffer.read()
	if res != 2:
		return [false, "read() = %s, expected %s" % [res, 2]]

	return [true, true]


func test_clear_does_nothing_on_empty_buffer(solution_script):
	var res = null
	var buffer = solution_script.CircularBuffer.new(1)

	buffer.clear()

	if not buffer.write(1):
		return [false, "write failed; expected success"]

	res = buffer.read()
	if res != 1:
		return [false, "read() = %s, expected %s" % [res, 1]]

	return [true, true]


func test_overwrite_acts_like_write_on_non_full_buffer(solution_script):
	var res = null
	var buffer = solution_script.CircularBuffer.new(2)

	if not buffer.write(1):
		return [false, "write failed; expected success"]

	buffer.overwrite(2)

	res = buffer.read()
	if res != 1:
		return [false, "read() = %s, expected %s" % [res, 1]]

	res = buffer.read()
	if res != 2:
		return [false, "read() = %s, expected %s" % [res, 2]]

	return [true, true]


func test_overwrite_replaces_the_oldest_item_on_full_buffer(solution_script):
	var res = null
	var buffer = solution_script.CircularBuffer.new(2)

	if not buffer.write(1):
		return [false, "write failed; expected success"]

	if not buffer.write(2):
		return [false, "write failed; expected success"]

	buffer.overwrite(3)

	res = buffer.read()
	if res != 2:
		return [false, "read() = %s, expected %s" % [res, 2]]

	res = buffer.read()
	if res != 3:
		return [false, "read() = %s, expected %s" % [res, 3]]

	return [true, true]


func test_overwrite_replaces_the_oldest_item_remaining_in_buffer_following_a_read(solution_script):
	var res = null
	var buffer = solution_script.CircularBuffer.new(3)

	if not buffer.write(1):
		return [false, "write failed; expected success"]

	if not buffer.write(2):
		return [false, "write failed; expected success"]

	if not buffer.write(3):
		return [false, "write failed; expected success"]

	res = buffer.read()
	if res != 1:
		return [false, "read() = %s, expected %s" % [res, 1]]

	if not buffer.write(4):
		return [false, "write failed; expected success"]

	buffer.overwrite(5)

	res = buffer.read()
	if res != 3:
		return [false, "read() = %s, expected %s" % [res, 3]]

	res = buffer.read()
	if res != 4:
		return [false, "read() = %s, expected %s" % [res, 4]]

	res = buffer.read()
	if res != 5:
		return [false, "read() = %s, expected %s" % [res, 5]]

	return [true, true]


func test_initial_clear_does_not_affect_wrapping_around(solution_script):
	var res = null
	var buffer = solution_script.CircularBuffer.new(2)

	buffer.clear()

	if not buffer.write(1):
		return [false, "write failed; expected success"]

	if not buffer.write(2):
		return [false, "write failed; expected success"]

	buffer.overwrite(3)

	buffer.overwrite(4)

	res = buffer.read()
	if res != 3:
		return [false, "read() = %s, expected %s" % [res, 3]]

	res = buffer.read()
	if res != 4:
		return [false, "read() = %s, expected %s" % [res, 4]]

	res = buffer.read()
	if res != null:
		return [false, "read() = %s, expected null" % res]

	return [true, true]
