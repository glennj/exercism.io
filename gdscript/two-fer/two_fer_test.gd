func test_no_name_given(solution_script):
	return [solution_script.two_fer(), "One for you, one for me."]

func test_a_name_given(solution_script):
	return [solution_script.two_fer("Alice"), "One for Alice, one for me."]

func test_another_name_given(solution_script):
	return [solution_script.two_fer("Bob"), "One for Bob, one for me."]
