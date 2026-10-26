func test_now_translates_to_two_hours_later(solution_script):
	var got = solution_script.delivery_date("2012-02-13T09:00:00", "NOW")
	var want = "2012-02-13T11:00:00"
	return [got, want]


func test_asap_before_one_in_the_afternoon_translates_to_today_at_five_in_the_afternoon(solution_script):
	var got = solution_script.delivery_date("1999-06-03T09:45:00", "ASAP")
	var want = "1999-06-03T17:00:00"
	return [got, want]


func test_asap_at_one_in_the_afternoon_translates_to_tomorrow_at_one_in_the_afternoon(solution_script):
	var got = solution_script.delivery_date("2008-12-21T13:00:00", "ASAP")
	var want = "2008-12-22T13:00:00"
	return [got, want]


func test_asap_after_one_in_the_afternoon_translates_to_tomorrow_at_one_in_the_afternoon(solution_script):
	var got = solution_script.delivery_date("2008-12-21T14:50:00", "ASAP")
	var want = "2008-12-22T13:00:00"
	return [got, want]


func test_eow_on_monday_translates_to_friday_at_five_in_the_afternoon(solution_script):
	var got = solution_script.delivery_date("2025-02-03T16:00:00", "EOW")
	var want = "2025-02-07T17:00:00"
	return [got, want]


func test_eow_on_tuesday_translates_to_friday_at_five_in_the_afternoon(solution_script):
	var got = solution_script.delivery_date("1997-04-29T10:50:00", "EOW")
	var want = "1997-05-02T17:00:00"
	return [got, want]


func test_eow_on_wednesday_translates_to_friday_at_five_in_the_afternoon(solution_script):
	var got = solution_script.delivery_date("2005-09-14T11:00:00", "EOW")
	var want = "2005-09-16T17:00:00"
	return [got, want]


func test_eow_on_thursday_translates_to_sunday_at_eight_in_the_evening(solution_script):
	var got = solution_script.delivery_date("2011-05-19T08:30:00", "EOW")
	var want = "2011-05-22T20:00:00"
	return [got, want]


func test_eow_on_friday_translates_to_sunday_at_eight_in_the_evening(solution_script):
	var got = solution_script.delivery_date("2022-08-05T14:00:00", "EOW")
	var want = "2022-08-07T20:00:00"
	return [got, want]


func test_eow_translates_to_leap_day(solution_script):
	var got = solution_script.delivery_date("2008-02-25T10:30:00", "EOW")
	var want = "2008-02-29T17:00:00"
	return [got, want]


func test_2m_before_the_second_month_of_this_year_translates_to_the_first_workday_of_the_second_month_of_this_year(solution_script):
	var got = solution_script.delivery_date("2007-01-02T14:15:00", "2M")
	var want = "2007-02-01T08:00:00"
	return [got, want]


func test_11m_in_the_eleventh_month_translates_to_the_first_workday_of_the_eleventh_month_of_next_year(solution_script):
	var got = solution_script.delivery_date("2013-11-21T15:30:00", "11M")
	var want = "2014-11-03T08:00:00"
	return [got, want]


func test_4m_in_the_ninth_month_translates_to_the_first_workday_of_the_fourth_month_of_next_year(solution_script):
	var got = solution_script.delivery_date("2019-11-18T15:15:00", "4M")
	var want = "2020-04-01T08:00:00"
	return [got, want]


func test_q1_in_the_first_quarter_translates_to_the_last_workday_of_the_first_quarter_of_this_year(solution_script):
	var got = solution_script.delivery_date("2003-01-01T10:45:00", "Q1")
	var want = "2003-03-31T08:00:00"
	return [got, want]


func test_q4_in_the_second_quarter_translates_to_the_last_workday_of_the_fourth_quarter_of_this_year(solution_script):
	var got = solution_script.delivery_date("2001-04-09T09:00:00", "Q4")
	var want = "2001-12-31T08:00:00"
	return [got, want]


func test_q3_in_the_fourth_quarter_translates_to_the_last_workday_of_the_third_quarter_of_next_year(solution_script):
	var got = solution_script.delivery_date("2022-10-06T11:00:00", "Q3")
	var want = "2023-09-29T08:00:00"
	return [got, want]


func test_q2_starting_in_the_last_month_of_the_second_quarter_translates_to_the_last_workday_of_the_second_quarter_of_this_year(solution_script):
	var got = solution_script.delivery_date("2019-06-15T09:50:00", "Q2")
	var want = "2019-06-28T08:00:00"
	return [got, want]
