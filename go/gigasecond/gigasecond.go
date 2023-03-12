package gigasecond

import "time"

var Gigasecond = 1_000_000_000 * time.Second

func AddGigasecond(t time.Time) time.Time {
	return t.Add(Gigasecond)
}
