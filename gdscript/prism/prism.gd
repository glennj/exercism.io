# A prism is an array [id, point, angle]

class Line:
	var point: Vector2
	var angle: float
	var theta: float

	func _init(point, angle):
		self.point = point
		self.angle = fmod(angle, 360)
		self.theta = deg_to_rad(self.angle)


	# is the prism p "inline" with this line
	func has(p):
		# determine the perpendicular distance
		var dist = (p[1].x - point.x) * sin(theta) - (p[1].y - point.y) * cos(theta)

		# epsilon value 0.0011 determined by trial and error
		return absf(dist) <= 0.0011


	func next_prism(prisms):
		var next = null
		var min = float(INT64_MAX)
		for p in prisms:
			var projection = (p[1].x - point.x) * cos(theta) + (p[1].y - point.y) * sin(theta)
			if projection > 0 and projection < min:
				min = projection
				next = p
		return next


# ------------------------------------------------------------
func find_sequence(position: Vector2, angle: float, prisms: Array):
	var line = Line.new(position, angle)
	var seq = []

	while true:
		var inline_prisms = prisms.filter(func (p): return line.has(p))

		var p = line.next_prism(inline_prisms)
		if not p: break

		seq.append(p[0])
		line = Line.new(p[1], line.angle + p[2])

	return seq
