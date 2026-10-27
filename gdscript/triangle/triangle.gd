func is_equilateral(sides):
	if not is_valid(sides): return false
	return sides[0] == sides[1] and sides[1] == sides[2]


func is_isosceles(sides):
	if not is_valid(sides): return false
	return sides[0] == sides[1] or sides[0] == sides[2] or sides[1] == sides[2]


func is_scalene(sides):
	if not is_valid(sides): return false
	return sides[0] != sides[1] and sides[0] != sides[2] and sides[1] != sides[2]


func is_valid(sides):
	sides.sort()
	return sides[0] > 0 and sides[0] + sides[1] > sides[2]
