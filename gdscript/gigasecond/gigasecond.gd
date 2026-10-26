const GIGASECOND = 1_000_000_000

func add(time):
	var unix = Time.get_unix_time_from_datetime_string(time)
	return Time.get_datetime_string_from_unix_time(unix + GIGASECOND)
