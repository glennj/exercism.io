func steps(number: int, moves := 0):
	if number < 1:
		return Error.ERR_INVALID_PARAMETER

	elif number == 1:
		return moves

	elif number % 2 == 0:
		return steps(number / 2, moves + 1)

	else:
		return steps(3 * number + 1, moves + 1)

