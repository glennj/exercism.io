func test_equilateral_all_sides_are_equal(solution_script):
	var args = [2, 2, 2]
	var expected = true
	var got = solution_script.is_equilateral(args)
	return [got, expected]


func test_equilateral_any_side_is_unequal(solution_script):
	var args = [2, 3, 2]
	var expected = false
	var got = solution_script.is_equilateral(args)
	return [got, expected]


func test_equilateral_no_sides_are_equal(solution_script):
	var args = [5, 4, 6]
	var expected = false
	var got = solution_script.is_equilateral(args)
	return [got, expected]


func test_equilateral_all_zero_sides_is_not_a_triangle(solution_script):
	var args = [0, 0, 0]
	var expected = false
	var got = solution_script.is_equilateral(args)
	return [got, expected]


func test_equilateral_sides_may_be_floats(solution_script):
	var args = [0.5, 0.5, 0.5]
	var expected = true
	var got = solution_script.is_equilateral(args)
	return [got, expected]


func test_isosceles_last_two_sides_are_equal(solution_script):
	var args = [3, 4, 4]
	var expected = true
	var got = solution_script.is_isosceles(args)
	return [got, expected]


func test_isosceles_first_two_sides_are_equal(solution_script):
	var args = [4, 4, 3]
	var expected = true
	var got = solution_script.is_isosceles(args)
	return [got, expected]


func test_isosceles_first_and_last_sides_are_equal(solution_script):
	var args = [4, 3, 4]
	var expected = true
	var got = solution_script.is_isosceles(args)
	return [got, expected]


func test_isosceles_equilateral_triangles_are_also_isosceles(solution_script):
	var args = [4, 4, 4]
	var expected = true
	var got = solution_script.is_isosceles(args)
	return [got, expected]


func test_isosceles_no_sides_are_equal(solution_script):
	var args = [2, 3, 4]
	var expected = false
	var got = solution_script.is_isosceles(args)
	return [got, expected]


func test_isosceles_first_triangle_inequality_violation(solution_script):
	var args = [1, 1, 3]
	var expected = false
	var got = solution_script.is_isosceles(args)
	return [got, expected]


func test_isosceles_second_triangle_inequality_violation(solution_script):
	var args = [1, 3, 1]
	var expected = false
	var got = solution_script.is_isosceles(args)
	return [got, expected]


func test_isosceles_third_triangle_inequality_violation(solution_script):
	var args = [3, 1, 1]
	var expected = false
	var got = solution_script.is_isosceles(args)
	return [got, expected]


func test_isosceles_sides_may_be_floats(solution_script):
	var args = [0.5, 0.4, 0.5]
	var expected = true
	var got = solution_script.is_isosceles(args)
	return [got, expected]


func test_scalene_no_sides_are_equal(solution_script):
	var args = [5, 4, 6]
	var expected = true
	var got = solution_script.is_scalene(args)
	return [got, expected]


func test_scalene_all_sides_are_equal(solution_script):
	var args = [4, 4, 4]
	var expected = false
	var got = solution_script.is_scalene(args)
	return [got, expected]


func test_scalene_first_and_second_sides_are_equal(solution_script):
	var args = [4, 4, 3]
	var expected = false
	var got = solution_script.is_scalene(args)
	return [got, expected]


func test_scalene_first_and_third_sides_are_equal(solution_script):
	var args = [3, 4, 3]
	var expected = false
	var got = solution_script.is_scalene(args)
	return [got, expected]


func test_scalene_second_and_third_sides_are_equal(solution_script):
	var args = [4, 3, 3]
	var expected = false
	var got = solution_script.is_scalene(args)
	return [got, expected]


func test_scalene_may_not_violate_triangle_inequality(solution_script):
	var args = [7, 3, 2]
	var expected = false
	var got = solution_script.is_scalene(args)
	return [got, expected]


func test_scalene_sides_may_be_floats(solution_script):
	var args = [0.5, 0.4, 0.6]
	var expected = true
	var got = solution_script.is_scalene(args)
	return [got, expected]
