func test_at_origin_facing_down(robot):
	robot.position = Vector2i(0, 0)
	robot.direction = Vector2i.DOWN
	var result = [robot.position, robot.direction]
	var expected = [Vector2i(0, 0), Vector2i.DOWN]
	return [result, expected]


func test_at_negative_position_facing_up(robot):
	robot.position = Vector2i(-1, -1)
	robot.direction = Vector2i.UP
	var result = [robot.position, robot.direction]
	var expected = [Vector2i(-1, -1), Vector2i.UP]
	return [result, expected]


func test_changes_down_to_right(robot):
	robot.position = Vector2i(0, 0)
	robot.direction = Vector2i.DOWN
	robot.move("R")
	var result = [robot.position, robot.direction]
	var expected = [Vector2i(0, 0), Vector2i.RIGHT]
	return [result, expected]


func test_changes_right_to_up(robot):
	robot.position = Vector2i(0, 0)
	robot.direction = Vector2i.RIGHT
	robot.move("R")
	var result = [robot.position, robot.direction]
	var expected = [Vector2i(0, 0), Vector2i.UP]
	return [result, expected]


func test_changes_up_to_left(robot):
	robot.position = Vector2i(0, 0)
	robot.direction = Vector2i.UP
	robot.move("R")
	var result = [robot.position, robot.direction]
	var expected = [Vector2i(0, 0), Vector2i.LEFT]
	return [result, expected]


func test_changes_left_to_down(robot):
	robot.position = Vector2i(0, 0)
	robot.direction = Vector2i.LEFT
	robot.move("R")
	var result = [robot.position, robot.direction]
	var expected = [Vector2i(0, 0), Vector2i.DOWN]
	return [result, expected]


func test_changes_down_to_left(robot):
	robot.position = Vector2i(0, 0)
	robot.direction = Vector2i.DOWN
	robot.move("L")
	var result = [robot.position, robot.direction]
	var expected = [Vector2i(0, 0), Vector2i.LEFT]
	return [result, expected]


func test_changes_left_to_up(robot):
	robot.position = Vector2i(0, 0)
	robot.direction = Vector2i.LEFT
	robot.move("L")
	var result = [robot.position, robot.direction]
	var expected = [Vector2i(0, 0), Vector2i.UP]
	return [result, expected]


func test_changes_up_to_right(robot):
	robot.position = Vector2i(0, 0)
	robot.direction = Vector2i.UP
	robot.move("L")
	var result = [robot.position, robot.direction]
	var expected = [Vector2i(0, 0), Vector2i.RIGHT]
	return [result, expected]


func test_changes_right_to_down(robot):
	robot.position = Vector2i(0, 0)
	robot.direction = Vector2i.RIGHT
	robot.move("L")
	var result = [robot.position, robot.direction]
	var expected = [Vector2i(0, 0), Vector2i.DOWN]
	return [result, expected]


func test_facing_down_increments_y(robot):
	robot.position = Vector2i(0, 0)
	robot.direction = Vector2i.DOWN
	robot.move("A")
	var result = [robot.position, robot.direction]
	var expected = [Vector2i(0, 1), Vector2i.DOWN]
	return [result, expected]


func test_facing_up_decrements_y(robot):
	robot.position = Vector2i(0, 0)
	robot.direction = Vector2i.UP
	robot.move("A")
	var result = [robot.position, robot.direction]
	var expected = [Vector2i(0, -1), Vector2i.UP]
	return [result, expected]


func test_facing_right_increments_x(robot):
	robot.position = Vector2i(0, 0)
	robot.direction = Vector2i.RIGHT
	robot.move("A")
	var result = [robot.position, robot.direction]
	var expected = [Vector2i(1, 0), Vector2i.RIGHT]
	return [result, expected]


func test_facing_left_decrements_x(robot):
	robot.position = Vector2i(0, 0)
	robot.direction = Vector2i.LEFT
	robot.move("A")
	var result = [robot.position, robot.direction]
	var expected = [Vector2i(-1, 0), Vector2i.LEFT]
	return [result, expected]


func test_moving_right_and_down_from_readme(robot):
	robot.position = Vector2i(7, 3)
	robot.direction = Vector2i.DOWN
	robot.move("RAALAL")
	var result = [robot.position, robot.direction]
	var expected = [Vector2i(9, 4), Vector2i.LEFT]
	return [result, expected]


func test_moving_left_and_down(robot):
	robot.position = Vector2i(0, 0)
	robot.direction = Vector2i.DOWN
	robot.move("LAAARALA")
	var result = [robot.position, robot.direction]
	var expected = [Vector2i(-4, 1), Vector2i.LEFT]
	return [result, expected]


func test_moving_left_and_up(robot):
	robot.position = Vector2i(2, -7)
	robot.direction = Vector2i.RIGHT
	robot.move("RRAAAAALA")
	var result = [robot.position, robot.direction]
	var expected = [Vector2i(-3, -8), Vector2i.UP]
	return [result, expected]


func test_moving_right_and_down(robot):
	robot.position = Vector2i(8, 4)
	robot.direction = Vector2i.UP
	robot.move("LAAARRRALLLL")
	var result = [robot.position, robot.direction]
	var expected = [Vector2i(11, 5), Vector2i.DOWN]
	return [result, expected]
