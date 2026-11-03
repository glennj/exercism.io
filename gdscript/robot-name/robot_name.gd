const LETTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"


class Robot:
	var name = null
	var rng = null

	func _init(rng):
		self.rng = rng
		self.rng.randomize()
		reset()

	func reset():
		name = LETTERS[rng.randi() % 26] \
				+ LETTERS[rng.randi() % 26] \
				+ ("%03d" % (rng.randi() % 1000))
