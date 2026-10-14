func _round(value):
	return snappedf(value, 0.01)



func test_age_on_earth(solution_script):
	var got = solution_script.on_planet("Earth", 1000000000)
	var want = 31.69
	return [_round(got), _round(want)]


func test_age_on_mercury(solution_script):
	var got = solution_script.on_planet("Mercury", 2134835688)
	var want = 280.88
	return [_round(got), _round(want)]


func test_age_on_venus(solution_script):
	var got = solution_script.on_planet("Venus", 189839836)
	var want = 9.78
	return [_round(got), _round(want)]


func test_age_on_mars(solution_script):
	var got = solution_script.on_planet("Mars", 2129871239)
	var want = 35.88
	return [_round(got), _round(want)]


func test_age_on_jupiter(solution_script):
	var got = solution_script.on_planet("Jupiter", 901876382)
	var want = 2.41
	return [_round(got), _round(want)]


func test_age_on_saturn(solution_script):
	var got = solution_script.on_planet("Saturn", 2000000000)
	var want = 2.15
	return [_round(got), _round(want)]


func test_age_on_uranus(solution_script):
	var got = solution_script.on_planet("Uranus", 1210123456)
	var want = 0.46
	return [_round(got), _round(want)]


func test_age_on_neptune(solution_script):
	var got = solution_script.on_planet("Neptune", 1821023456)
	var want = 0.35
	return [_round(got), _round(want)]


func test_invalid_planet_causes_error(solution_script):
	var got = solution_script.on_planet("Sun", 680804807)
	var want = null
	return [got, want]
