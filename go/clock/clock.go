package clock

import "fmt"

const minutesPerDay = 24 * 60

type Clock struct {
	minutes int
}

func New(h, m int) Clock {
	return Clock{minutes: normalize(h * 60 + m)}
}

// normalize maintains the minutes as: 0 <= m < minutesPerDay
func normalize(m int) int {
	return (m % minutesPerDay + minutesPerDay) % minutesPerDay
}

func (c Clock) Add(m int) Clock {
	c.minutes = normalize(c.minutes + m)
	return c
}

func (c Clock) Subtract(m int) Clock {
	return c.Add(-m)
}

func (c Clock) String() string {
	return fmt.Sprintf("%02d:%02d", c.minutes / 60, c.minutes % 60)
}

/* benchmark this implementation only
 * no allocations per op, so quite speedy
 *
 * BenchmarkAddMinutes             264548541                4.522 ns/op           0 B/op          0 allocs/op
 * BenchmarkSubtractMinutes        290562915                4.022 ns/op           0 B/op          0 allocs/op
 * BenchmarkCreateClocks           98426277                11.96 ns/op            0 B/op          0 allocs/op
 */
