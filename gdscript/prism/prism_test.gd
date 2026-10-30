func test_zero_prisms(solution_script):
	var position = Vector2(0, 0)
	var angle = 0
	var prisms =  [
	]

	var want = []
	var got = solution_script.find_sequence(position, angle, prisms)
	return [got, want]


func test_one_prism_one_hit(solution_script):
	var position = Vector2(0, 0)
	var angle = 0
	var prisms =  [
		[1, Vector2(10, 0), 0],
	]

	var want = [1]
	var got = solution_script.find_sequence(position, angle, prisms)
	return [got, want]


func test_one_prism_zero_hits(solution_script):
	var position = Vector2(0, 0)
	var angle = 0
	var prisms =  [
		[1, Vector2(-10, 0), 0],
	]

	var want = []
	var got = solution_script.find_sequence(position, angle, prisms)
	return [got, want]


func test_going_up_zero_hits(solution_script):
	var position = Vector2(0, 0)
	var angle = 90
	var prisms =  [
		[3, Vector2(0, -10), 0],
		[1, Vector2(-10, 0), 0],
		[2, Vector2(10, 0), 0],
	]

	var want = []
	var got = solution_script.find_sequence(position, angle, prisms)
	return [got, want]


func test_going_down_zero_hits(solution_script):
	var position = Vector2(0, 0)
	var angle = -90
	var prisms =  [
		[1, Vector2(10, 0), 0],
		[2, Vector2(0, 10), 0],
		[3, Vector2(-10, 0), 0],
	]

	var want = []
	var got = solution_script.find_sequence(position, angle, prisms)
	return [got, want]


func test_going_left_zero_hits(solution_script):
	var position = Vector2(0, 0)
	var angle = 180
	var prisms =  [
		[2, Vector2(0, 10), 0],
		[3, Vector2(10, 0), 0],
		[1, Vector2(0, -10), 0],
	]

	var want = []
	var got = solution_script.find_sequence(position, angle, prisms)
	return [got, want]


func test_negative_angle(solution_script):
	var position = Vector2(0, 0)
	var angle = -180
	var prisms =  [
		[1, Vector2(0, -10), 0],
		[2, Vector2(0, 10), 0],
		[3, Vector2(10, 0), 0],
	]

	var want = []
	var got = solution_script.find_sequence(position, angle, prisms)
	return [got, want]


func test_large_angle(solution_script):
	var position = Vector2(0, 0)
	var angle = 2340
	var prisms =  [
		[1, Vector2(10, 0), 0],
	]

	var want = []
	var got = solution_script.find_sequence(position, angle, prisms)
	return [got, want]


func test_upward_refraction_two_hits(solution_script):
	var position = Vector2(0, 0)
	var angle = 0
	var prisms =  [
		[1, Vector2(10, 10), 0],
		[2, Vector2(10, 0), 90],
	]

	var want = [2, 1]
	var got = solution_script.find_sequence(position, angle, prisms)
	return [got, want]


func test_downward_refraction_two_hits(solution_script):
	var position = Vector2(0, 0)
	var angle = 0
	var prisms =  [
		[1, Vector2(10, 0), -90],
		[2, Vector2(10, -10), 0],
	]

	var want = [1, 2]
	var got = solution_script.find_sequence(position, angle, prisms)
	return [got, want]


func test_same_prism_twice(solution_script):
	var position = Vector2(0, 0)
	var angle = 0
	var prisms =  [
		[2, Vector2(10, 0), 0],
		[1, Vector2(20, 0), -180],
	]

	var want = [2, 1, 2]
	var got = solution_script.find_sequence(position, angle, prisms)
	return [got, want]


func test_simple_path(solution_script):
	var position = Vector2(0, 0)
	var angle = 0
	var prisms =  [
		[3, Vector2(30, 10), 45],
		[1, Vector2(10, 10), -90],
		[2, Vector2(10, 0), 90],
		[4, Vector2(20, 0), 0],
	]

	var want = [2, 1, 3]
	var got = solution_script.find_sequence(position, angle, prisms)
	return [got, want]


func test_multiple_prisms_floating_point_precision(solution_script):
	var position = Vector2(0, 0)
	var angle = -6.429
	var prisms =  [
		[26, Vector2(5.8, 73.4), 6.555],
		[24, Vector2(36.2, 65.2), -0.304],
		[20, Vector2(20.4, 82.8), 45.17],
		[31, Vector2(-20.2, 48.8), 30.615],
		[30, Vector2(24.0, 0.6), 28.771],
		[29, Vector2(31.4, 79.4), 61.327],
		[28, Vector2(36.4, 31.4), -18.157],
		[22, Vector2(47.0, 57.8), 54.745],
		[38, Vector2(36.4, 79.2), 49.05],
		[10, Vector2(37.8, 55.2), 11.978],
		[18, Vector2(-26.0, 42.6), 22.661],
		[25, Vector2(38.8, 76.2), 51.958],
		[2, Vector2(0.0, 42.4), -21.817],
		[35, Vector2(21.4, 44.8), -171.579],
		[7, Vector2(14.2, -1.6), 19.081],
		[33, Vector2(11.2, 44.4), -165.941],
		[11, Vector2(15.4, 82.6), 66.262],
		[16, Vector2(30.8, 6.6), 35.852],
		[15, Vector2(-3.0, 79.2), 53.782],
		[4, Vector2(29.0, 75.4), 17.016],
		[23, Vector2(41.6, 59.8), 70.763],
		[8, Vector2(-10.0, 15.8), -9.24],
		[13, Vector2(48.6, 51.8), 45.812],
		[1, Vector2(13.2, 77.0), 17.937],
		[34, Vector2(-8.8, 36.8), -4.199],
		[21, Vector2(24.4, 75.8), 20.783],
		[17, Vector2(-4.4, 74.6), 24.709],
		[9, Vector2(30.8, 41.8), -165.413],
		[32, Vector2(4.2, 78.6), 40.892],
		[37, Vector2(-15.8, 47.0), 33.29],
		[6, Vector2(1.0, 80.6), 51.295],
		[36, Vector2(-27.0, 47.8), 92.52],
		[14, Vector2(-2.0, 34.4), -52.001],
		[5, Vector2(23.2, 80.2), 31.866],
		[27, Vector2(-5.6, 32.8), -75.303],
		[12, Vector2(-1.0, 0.2), 0.0],
		[3, Vector2(-6.6, 3.2), 46.72],
		[19, Vector2(-13.8, 24.2), -9.205],
	]

	var want = [7, 30, 16, 28, 13, 22, 23, 10, 9, 24, 25, 38, 29, 4, 35, 21, 5, 20, 11, 1, 33, 26, 32, 6, 15, 17, 2, 14, 27, 34, 37, 31, 36, 18, 19, 8, 3, 12]
	var got = solution_script.find_sequence(position, angle, prisms)
	return [got, want]


func test_complex_path_with_multiple_prisms_floating_point_precision(solution_script):
	var position = Vector2(0, 0)
	var angle = 0.0
	var prisms =  [
		[46, Vector2(37.4, 20.6), -88.332],
		[72, Vector2(-24.2, 23.4), -90.774],
		[25, Vector2(78.6, 7.8), 98.562],
		[60, Vector2(-58.8, 31.6), 115.56],
		[22, Vector2(75.2, 28.0), 63.515],
		[2, Vector2(89.8, 27.8), 91.176],
		[23, Vector2(9.8, 30.8), 30.829],
		[69, Vector2(22.8, 20.6), -88.315],
		[44, Vector2(-0.8, 15.6), -116.565],
		[36, Vector2(-24.2, 8.2), -90.0],
		[53, Vector2(-1.2, 0.0), 0.0],
		[52, Vector2(14.2, 24.0), -143.896],
		[5, Vector2(-65.2, 21.6), 93.128],
		[66, Vector2(5.4, 15.6), 31.608],
		[51, Vector2(-72.6, 21.0), -100.976],
		[65, Vector2(48.0, 10.2), 87.455],
		[21, Vector2(-41.8, 0.0), 68.352],
		[18, Vector2(-46.2, 19.2), -128.362],
		[10, Vector2(74.4, 0.4), 90.939],
		[15, Vector2(67.6, 0.4), 84.958],
		[35, Vector2(14.8, -0.4), 89.176],
		[1, Vector2(83.0, 0.2), 89.105],
		[68, Vector2(14.6, 28.0), -29.867],
		[67, Vector2(79.8, 18.6), -136.643],
		[38, Vector2(53.0, 14.6), -90.848],
		[31, Vector2(-58.0, 6.6), -61.837],
		[74, Vector2(-30.8, 0.4), 85.966],
		[48, Vector2(-4.6, 10.0), -161.222],
		[12, Vector2(59.0, 5.0), -91.164],
		[33, Vector2(-16.4, 18.4), 90.734],
		[4, Vector2(82.6, 27.6), 71.127],
		[75, Vector2(-10.2, 30.6), -1.108],
		[28, Vector2(38.0, 0.0), 86.863],
		[11, Vector2(64.4, -0.2), 92.353],
		[9, Vector2(-51.4, 31.6), 67.249],
		[26, Vector2(-39.8, 30.8), 61.113],
		[30, Vector2(-34.2, 0.6), 111.33],
		[56, Vector2(-51.0, 0.2), 70.445],
		[41, Vector2(-12.0, 0.0), 91.219],
		[24, Vector2(63.8, 14.4), 86.586],
		[70, Vector2(-72.8, 13.4), -87.238],
		[3, Vector2(22.4, 7.0), -91.685],
		[13, Vector2(34.4, 7.0), 90.0],
		[16, Vector2(-47.4, 11.4), -136.02],
		[6, Vector2(90.0, 0.2), 90.415],
		[54, Vector2(44.0, 27.8), 85.969],
		[32, Vector2(-9.0, 0.0), 91.615],
		[8, Vector2(-31.6, 30.8), 0.535],
		[39, Vector2(-12.0, 8.2), 90.0],
		[14, Vector2(-79.6, 32.4), 92.342],
		[42, Vector2(65.8, 20.8), -85.867],
		[40, Vector2(-65.0, 14.0), 87.109],
		[45, Vector2(10.6, 18.8), 23.697],
		[71, Vector2(-24.2, 18.6), -88.531],
		[7, Vector2(-72.6, 6.4), -89.148],
		[62, Vector2(-32.0, 24.8), -140.8],
		[49, Vector2(34.4, -0.2), 89.415],
		[63, Vector2(74.2, 12.6), -138.429],
		[59, Vector2(82.8, 13.0), -140.177],
		[34, Vector2(-9.4, 23.2), -88.238],
		[76, Vector2(-57.6, 0.0), 1.2],
		[43, Vector2(7.0, 0.0), 116.565],
		[20, Vector2(45.8, -0.2), 1.469],
		[37, Vector2(-16.6, 13.2), 84.785],
		[58, Vector2(-79.0, -0.2), 89.481],
		[50, Vector2(-24.2, 12.8), -86.987],
		[64, Vector2(59.2, 10.2), -92.203],
		[61, Vector2(-72.0, 26.4), -83.66],
		[47, Vector2(45.4, 5.8), -82.992],
		[17, Vector2(-52.2, 17.8), -52.938],
		[57, Vector2(-61.8, 32.0), 84.627],
		[29, Vector2(47.2, 28.2), 92.954],
		[27, Vector2(-4.6, 0.2), 87.397],
		[55, Vector2(-61.4, 26.4), 94.086],
		[73, Vector2(-40.4, 13.4), -62.229],
		[19, Vector2(53.2, 20.6), -87.181],
	]

	var want = [43, 44, 66, 45, 52, 35, 49, 13, 3, 69, 46, 28, 20, 11, 24, 38, 19, 42, 15, 10, 63, 25, 59, 1, 6, 2, 4, 67, 22, 29, 65, 64, 12, 47, 54, 68, 23, 75, 8, 26, 18, 9, 60, 17, 31, 7, 70, 40, 5, 51, 61, 55, 57, 14, 58, 76, 56, 16, 21, 30, 73, 62, 74, 41, 39, 36, 50, 37, 33, 71, 72, 34, 32, 27, 48, 53]
	var got = solution_script.find_sequence(position, angle, prisms)
	return [got, want]
