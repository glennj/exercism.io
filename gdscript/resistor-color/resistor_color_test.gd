func test_black(solution_script):
	return [solution_script.color_code("black"), 0]

func test_white(solution_script):
	return [solution_script.color_code("white"), 9]

func test_orange(solution_script):
	return [solution_script.color_code("orange"), 3]

func test_colors(solution_script):
	var colors = [
		"black",
		"brown",
		"red",
		"orange",
		"yellow",
		"green",
		"blue",
		"violet",
		"grey",
		"white"
	]
	return [solution_script.colors(), colors]
