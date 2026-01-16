package difference_of_squares

import "core:math"

square_of_sum :: proc (n: int) -> int {
	sum := n * (n + 1) / 2
	return sum * sum
}

sum_of_squares :: proc (n: int) -> int {
	return n * (n + 1) * (2 * n + 1) / 6
}

difference :: proc (n: int) -> int {
	return math.abs(sum_of_squares(n) - square_of_sum(n))
}
