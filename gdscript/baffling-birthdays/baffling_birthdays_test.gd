func test_one_birthdate(solution_script):
	var got = solution_script.shared_birthday(["2000-01-01"])
	var want = false
	return [got, want]


func test_two_birthdates_with_same_year_month_and_day(solution_script):
	var got = solution_script.shared_birthday(["2000-01-01", "2000-01-01"])
	var want = true
	return [got, want]


func test_two_birthdates_with_same_year_and_month_but_different_day(solution_script):
	var got = solution_script.shared_birthday(["2012-05-09", "2012-05-17"])
	var want = false
	return [got, want]


func test_two_birthdates_with_same_month_and_day_but_different_year(solution_script):
	var got = solution_script.shared_birthday(["1999-10-23", "1988-10-23"])
	var want = true
	return [got, want]


func test_two_birthdates_with_same_year_but_different_month_and_day(solution_script):
	var got = solution_script.shared_birthday(["2007-12-19", "2007-04-27"])
	var want = false
	return [got, want]


func test_two_birthdates_with_different_year_month_and_day(solution_script):
	var got = solution_script.shared_birthday(["1997-08-04", "1963-11-23"])
	var want = false
	return [got, want]


func test_multiple_birthdates_without_shared_birthday(solution_script):
	var got = solution_script.shared_birthday(["1966-07-29", "1977-02-12", "2001-12-25", "1980-11-10"])
	var want = false
	return [got, want]


func test_multiple_birthdates_with_one_shared_birthday(solution_script):
	var got = solution_script.shared_birthday(["1966-07-29", "1977-02-12", "2001-07-29", "1980-11-10"])
	var want = true
	return [got, want]


func test_multiple_birthdates_with_more_than_one_shared_birthday(solution_script):
	var got = solution_script.shared_birthday(["1966-07-29", "1977-02-12", "2001-12-25", "1980-07-29", "2019-02-12"])
	var want = true
	return [got, want]


func test_random_birthdates_count_0(solution_script):
	var got = len(solution_script.random_birthdates(0))
	var want = 0
	return [got, want]


func test_random_birthdates_count_1(solution_script):
	var got = len(solution_script.random_birthdates(1))
	var want = 1
	return [got, want]


func test_random_birthdates_count_2(solution_script):
	var got = len(solution_script.random_birthdates(2))
	var want = 2
	return [got, want]


func test_random_birthdates_count_5(solution_script):
	var got = len(solution_script.random_birthdates(5))
	var want = 5
	return [got, want]


func test_random_birthdates_count_10(solution_script):
	var got = len(solution_script.random_birthdates(10))
	var want = 10
	return [got, want]


func test_random_birthdates_not_leapyear(solution_script):
	var is_leap = false
	var dates = solution_script.random_birthdates(300)
	for date in dates:
		if date["year"] % 400 == 0 or date["year"] % 4 == 0 and date["year"] % 100 != 0:
			is_leap = true
			break
	return [is_leap, false]


func test_random_birthdates_random_months(solution_script):
	var months = {}
	var dates = solution_script.random_birthdates(600)
	for date in dates:
		if date["month"] not in months:
			months[date["month"]] = 0
		months[date["month"]] += 1
	var is_distributed = len(months) == 12
	for month in months:
		if months[month] < 15:
			is_distributed = false

	return [is_distributed, true]


func test_random_birthdates_random_days(solution_script):
	var days = {}
	var dates = solution_script.random_birthdates(600)
	for date in dates:
		if date["day"] not in days:
			days[date["day"]] = 0
		days[date["day"]] += 1
	var is_distributed = len(days) > 25
	for day in days:
		if days[day] < 5:
			is_distributed = false

	return [is_distributed, true]


func test_for_one_person(solution_script):
	var got = solution_script.estimated_probability_of_shared_birthday(1)
	var want = 0.0
	return [got - 1 < want and want < got + 1, true]


func test_among_ten_people(solution_script):
	var got = solution_script.estimated_probability_of_shared_birthday(10)
	var want = 11.694818
	return [got - 1 < want and want < got + 1, true]


func test_among_twenty_three_people(solution_script):
	var got = solution_script.estimated_probability_of_shared_birthday(23)
	var want = 50.729723
	return [got - 1 < want and want < got + 1, true]


func test_among_seventy_people(solution_script):
	var got = solution_script.estimated_probability_of_shared_birthday(70)
	var want = 99.915958
	return [got - 1 < want and want < got + 1, true]
