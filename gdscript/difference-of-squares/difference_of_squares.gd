func square_of_sum(n: int) -> int:
	return (n * (n + 1) / 2) ** 2


func sum_of_squares(n: int) -> int:
	return n * (n + 1) * (2 * n + 1) / 6


func difference_of_squares(n: int) -> int:
	return abs(sum_of_squares(n) - square_of_sum(n))
