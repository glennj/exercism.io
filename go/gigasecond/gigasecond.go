package gigasecond

import (
	"strconv"
	"time"
)

var Gigasecond, _ = time.ParseDuration(strconv.Itoa(1_000_000_000) + "s")

func AddGigasecond(t time.Time) time.Time {
	return t.Add(Gigasecond)
}
