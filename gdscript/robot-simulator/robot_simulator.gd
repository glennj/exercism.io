@export var position: Vector2i
@export var direction: Vector2i

# Directions are represented as follows:
# north -> Vector2i.DOWN  == Vector2i(0, 1)
# east  -> Vector2i.RIGHT == Vector2i(1, 0)
# south -> Vector2i.UP    == Vector2i(0, -1)
# west  -> Vector2i.LEFT  == Vector2i(-1, 0)


func move(instructions: String):
	for i in instructions:
		match i:
			"L": _left_turn()
			"R": _right_turn()
			"A": _advance()
			_:   pass


func _left_turn():
	match direction:
		Vector2i.DOWN:  direction = Vector2i.LEFT	
		Vector2i.UP:    direction = Vector2i.RIGHT
		Vector2i.LEFT:  direction = Vector2i.UP
		Vector2i.RIGHT: direction = Vector2i.DOWN


func _right_turn():
	match direction:
		Vector2i.DOWN:  direction = Vector2i.RIGHT
		Vector2i.UP:    direction = Vector2i.LEFT
		Vector2i.LEFT:  direction = Vector2i.DOWN
		Vector2i.RIGHT: direction = Vector2i.UP


func _advance():
	position += direction
