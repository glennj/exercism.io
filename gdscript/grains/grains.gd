const SIZE = 63


func square(num: int):
	if (num < 1 or num > SIZE):
		return null

	return 1 << (num - 1)


func total():
	return (1 << SIZE) - 1
