class Bucket:
	var name = ""
	var size = 0
	var amount = 0

	func _init(a_name, a_size):
		name = a_name
		size = a_size

	func is_full():
		return amount == size

	func is_empty():
		return amount == 0

	func fill():
		amount = size

	func empty():
		amount = 0

	func add(qty):
		amount += qty

	func pour_into(other):
		var qty = [amount, other.size - other.amount].min()
		add(-qty)
		other.add(qty)

	func dump():
		return {name = name, size = size, amt = amount}


# ------------------------------------------------------------
func measure(one: int, two: int, goal: int, start: String):
	if not valid(one, two, goal):
		return null

	var b1 = Bucket.new("one", one)
	var b2 = Bucket.new("two", two)

	if start == "one":
		return solve(b1, b2, goal)
	else:
		return solve(b2, b1, goal)


func valid(size1, size2, goal):
	if goal > [size1, size2].max():
		return false

	var g = gcd(size1, size2)
	if g != 1 and goal % g != 0:
		return false

	return true


func gcd(a, b):
	return a if b == 0 else gcd(b, a % b)


func result(winner, loser, moves):
	return {
		"moves": moves,
		"goal_bucket": winner.name,
		"other_bucket": loser.amount,
	}


func solve(start, other, goal):
	var moves = 0

	start.fill()
	moves += 1

	if other.size == goal and start.size != goal:
		other.fill()
		moves += 1

	while true:
		if start.amount == goal:
			return result(start, other, moves)
		if other.amount == goal:
			return result(other, start, moves)

		if start.is_empty():  start.fill()
		elif other.is_full(): other.empty()
		else:                 start.pour_into(other)

		moves += 1
