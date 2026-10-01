func test_say_hi_(solution_script):
	var expected = "Hello, World!"
	var got = solution_script.hello()
	return [got, expected]
