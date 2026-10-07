func is_armstrong_number(number):
	return number == armstrong_sum(number)

func armstrong_sum(number):
	var width = 1 + floor(log(number) / log(10))
	var sum = 0
	while number > 0:
		sum += (number % 10) ** width
		number /= 10
	return sum
