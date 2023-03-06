package raindrops

import (
	"strconv"
	"strings"
)

// Convert returns the raindrop sounds of a number.
func Convert(number int) string {
	drops := make([]string, 0, 3)

	if number%3 == 0 {
		drops = append(drops, "Pling")
	}
	if number%5 == 0 {
		drops = append(drops, "Plang")
	}
	if number%7 == 0 {
		drops = append(drops, "Plong")
	}

	if len(drops) == 0 {
		drops = append(drops, strconv.Itoa(number))
	}

	return strings.Join(drops, "")
}

/*	benchmarking, the string-only version is a little bit slower
 * 
 * arrays:
 * BenchmarkConvert         3220380               402.8 ns/op            64 B/op          4 allocs/op
 * strings:
 * BenchmarkConvert         2349109               525.7 ns/op            80 B/op          5 allocs/op
 *
 * also, if I don't give the drops array a capacity `drops := []string{}`, it's _much_ slower
 * BenchmarkConvert          696098              1687 ns/op             496 B/op         24 allocs/op
 */

/*
import "strconv"

func Convert(number int) string {
	drops := ""

	if number%3 == 0 {
		drops += "Pling"
	}
	if number%5 == 0 {
		drops += "Plang"
	}
	if number%7 == 0 {
		drops += "Plong"
	}

	if len(drops) == 0 {
		drops = strconv.Itoa(number)
	}

    return drops
}
*/
