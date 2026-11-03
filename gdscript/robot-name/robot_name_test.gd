var name_re = RegEx.create_from_string("^[A-Z]{2}[0-9]{3}$")
var rng = RandomNumberGenerator.new()


func name_is_valid(name):
	return name != null and name_re.search(name) != null


func test_valid_name(solution_script):
	var robot = solution_script.Robot.new(rng)
	var got = name_is_valid(robot.name)
	return [got, true]


func test_different_names(solution_script):
	var saved_state = rng.state
	var robot1 = solution_script.Robot.new(rng)
	rng.state = saved_state
	var robot2 = solution_script.Robot.new(rng)

	var got = robot1.name != robot2.name
	return [got, true]


func test_reset_name(solution_script):
	var saved_state = rng.state
	var robot = solution_script.Robot.new(rng)
	var first_name = robot.name
	rng.state = saved_state
	robot.reset()
	var second_name = robot.name

	var got = first_name != second_name and name_is_valid(first_name) and name_is_valid(second_name)
	return [got, true]
