// Package gigasecond should have a package comment
package gigasecond

import "time"

// AddGigasecond should have a comment documenting it.
func AddGigasecond(t time.Time) time.Time {
	/*
	 * gigasecond, err := time.ParseDuration("1000000000s")
	 * if err != nil {
	 * 	panic("Can't parse 1000000000s")
	 * }
	 * below is better
	 */
	gigasecond := 1e9 * time.Second
	return t.Add(gigasecond)
}
