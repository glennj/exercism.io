func shared_birthday(birthdays):
	var seen = {}
	for d in birthdays:
		var month_day = d.substr(5)
		if seen.has(month_day):
			return true
		seen[month_day] = true
	return false


func random_birthdates(count):
	var is_leap = func(y): 
		return y % 4 == 0 and (y % 100 != 0 or y % 400 == 0)

	var days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

	var rand_date = func(_unused):
		var year
		while true:
			year = 1900 + (randi() % 120)
			if not is_leap.call(year):
				break
		var month = randi() % 12
		var day = randi() % days[month]
		return {year = year, month = month + 1, day = day + 1}

	return range(count).map(rand_date)


func estimated_probability_of_shared_birthday(count):
	# https://en.wikipedia.org/wiki/Birthday_problem#Approximations
    return 100 * (1 - exp(1.0 * -count * (count - 1) / 730))
