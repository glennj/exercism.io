package resistorcolortrio

import (
    "resistorcolor"
    "resistorcolorduo"
    "math"
    "fmt"
)

// Label describes the resistance value given the colors of a resistor.
// The label is a string with a resistance value with an unit appended
// (e.g. "33 ohms", "470 kiloohms").
func Label(colors []string) string {
    value := resistorcolorduo.Value(colors[0:2])
    value *= int(math.Pow10(resistorcolor.ColorCode(colors[2])))

    value, prefix := getPrefix(value)
    return fmt.Sprintf("%d %s%s", value, prefix, "ohms")
}

var magnitudes = []int{1_000_000_000, 1_000_000, 1_000}
var prefixes = []string{"giga", "mega", "kilo"}

func getPrefix(value int) (int, string) {
    if value > 0 {
        for i, magnitude := range magnitudes {
            if value % magnitude == 0 {
                return value / magnitude, prefixes[i]
            }
        }
    }
    return value, ""
}
