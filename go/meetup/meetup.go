package meetup

import "time"

type WeekSchedule int

const (
	First WeekSchedule = iota
	Second
	Third
	Fourth
	Teenth
	Last
)

func Day(wSched WeekSchedule, wDay time.Weekday, month time.Month, year int) int {
	days := weekdayMonthDays(year, month, wDay)

	switch wSched {
	case Teenth:
		// The teenth day will be the 2nd or 3rd entry
		if days[1] < 13 {
			return days[2]
		}
		return days[1]
	case Last:
		return days[len(days)-1]
	default:
		return days[wSched]
	}
}

func weekdayMonthDays(year int, month time.Month, wDay time.Weekday) []int {
	oneDay, _ := time.ParseDuration("24h")
	oneWeek, _ := time.ParseDuration("168h") // 7 days

	// 1. find the first day of the month with the given weekday
	date := time.Date(year, month, 1, 0, 0, 0, 0, time.UTC)
	for date.Weekday() != wDay {
		date = date.Add(oneDay)
	}

	// 2. collect all the days in the month with the given weekday
	days := make([]int, 0, 5)
	for date.Month() == month {
		days = append(days, date.Day())
		date = date.Add(oneWeek)
	}

	return days
}

// bench
// BenchmarkDay-2             32343             36276 ns/op            4560 B/op         95 allocs/op
