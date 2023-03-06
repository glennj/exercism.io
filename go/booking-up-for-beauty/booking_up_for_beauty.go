package booking

import "time"

const shortLayout = "1/2/2006 15:04:05"
const mediumLayout = "January 2, 2006 15:04:05"
const longLayout = "Monday, January 2, 2006 15:04:05"

// Schedule returns a time.Time from a string containing a date.
// Example:
//
//	Schedule("7/25/2019 13:45:00")
func Schedule(date string) time.Time {
	t, err := time.Parse(shortLayout, date)
	if err != nil {
		panic(err)
	}
	return t
}

// HasPassed returns whether a date has passed.
// Example:
//
//	HasPassed("July 25, 2019 13:45:00")
func HasPassed(date string) bool {
	t, err := time.Parse(mediumLayout, date)
	if err != nil {
		panic(err)
	}
	return t.Unix() < time.Now().Unix()
}

// IsAfternoonAppointment returns whether a time is in the afternoon.
// Example:
//
//	IsAfternoonAppointment("Thursday, July 25, 2019 13:45:00")
func IsAfternoonAppointment(date string) bool {
	t, err := time.Parse(longLayout, date)
	if err != nil {
		panic(err)
	}
	hr := t.Hour()
	return 12 <= hr && hr < 18
}

// Description returns a formatted string of the appointment time.
// Example:
//
//	Description("7/25/2019 13:45:00")
//	=> "You have an appointment on Thursday, July 25, 2019, at 13:45."
func Description(date string) string {
	t, err := time.Parse(shortLayout, date)
	if err != nil {
		panic(err)
	}
	apptLayout := "You have an appointment on Monday, January 2, 2006, at 15:04."
	return t.Format(apptLayout)
}

// AnniversaryDate returns a Time with this year's anniversary.
// > a beauty salon that opened on September 15th in 2012.
func AnniversaryDate() time.Time {
	return time.Date(time.Now().Year(), 9, 15, 0, 0, 0, 0, time.UTC)
}
