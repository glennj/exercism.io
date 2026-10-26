func delivery_date(start: String, description: String):
	match description:
		"NOW":  return now(start)
		"ASAP": return asap(start)
		"EOW":  return eow(start)
		_ when description.ends_with("M"):
			var m = description.to_int()
			return month(m, start)
		_ when description.begins_with("Q"):
			var q = description.substr(1, -1).to_int()
			return quarter(q, start)


# ------------------------------------------------------------
func now(start):
	var dt = datetime_string_to_dict(start)

	dt.hour += 2

	return datetime_dict_to_string(dt)


func asap(start):
	var dt = datetime_string_to_dict(start)

	if dt.hour < 13:
		dt.hour = 17
	else:
		dt.hour = 13
		dt.day += 1
	dt.minute = 0

	return datetime_dict_to_string(dt)


func eow(start):
	var dt = datetime_string_to_dict(start)

	match dt.weekday:
		Time.Weekday.WEEKDAY_MONDAY, Time.Weekday.WEEKDAY_TUESDAY, Time.Weekday.WEEKDAY_WEDNESDAY:
			dt.day += delta_days(Time.Weekday.WEEKDAY_FRIDAY, dt.weekday)
			dt.hour = 17
		_:
			dt.day += delta_days(Time.Weekday.WEEKDAY_SUNDAY, dt.weekday)
			dt.hour = 20
	dt.minute = 0

	return datetime_dict_to_string(dt)


func month(m, start):
	var dt = datetime_string_to_dict(start)

	if dt.month >= m:
		dt.year += 1

	dt.month = m
	dt.day = 1
	dt.hour = 8
	dt.minute = 0

	dt = refresh_datetime_dict(dt)

	# first workday
	match dt.weekday:
		Time.Weekday.WEEKDAY_SATURDAY: dt.day += 2
		Time.Weekday.WEEKDAY_SUNDAY:   dt.day += 1

	return datetime_dict_to_string(dt)


func quarter(q, start):
	var dt = datetime_string_to_dict(start)

	var m = q * 3
	if dt.month > m:
		dt.year += 1

	dt.month = m
	dt.day = days_in_month(dt.year, dt.month)
	dt.hour = 8
	dt.minute = 0

	dt = refresh_datetime_dict(dt)

	# last workday
	match dt.weekday:
		Time.Weekday.WEEKDAY_SATURDAY: dt.day -= 1
		Time.Weekday.WEEKDAY_SUNDAY:   dt.day -= 2

	return datetime_dict_to_string(dt)


# ------------------------------------------------------------
func datetime_string_to_dict(str):
	var unix = Time.get_unix_time_from_datetime_string(str)
	return Time.get_datetime_dict_from_unix_time(unix)


func datetime_dict_to_string(dt):
	dt = normalize_datetime_dict(dt)
	return Time.get_datetime_string_from_datetime_dict(dt, false)


# typically when we need to correct the weekday, refresh the dict
# by converting to unix time, then convert back to dict
func refresh_datetime_dict(dt):
	var unix = Time.get_unix_time_from_datetime_dict(normalize_datetime_dict(dt))
	return Time.get_datetime_dict_from_unix_time(unix)


func delta_days(d1, d2):
	return (d1 - d2 + 7) % 7


const DAYS_PER_MONTH = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]


func days_in_month(year, month):
	var days = DAYS_PER_MONTH[month]

	if month == 2 and year % 4 == 0 and (year % 100 != 0 or year % 400 == 0):
		days += 1
	
	return days


func normalize_datetime_dict(dt):
	var days = days_in_month(dt.year, dt.month)

	if dt.day > days:
		dt.month += 1
		dt.day -= days

	if dt.month > 12:
		dt.year += 1
		dt.month -= 12

	return dt
