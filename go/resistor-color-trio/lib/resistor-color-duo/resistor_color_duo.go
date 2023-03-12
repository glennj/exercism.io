package resistorcolorduo

import "resistorcolor"

// Value should return the resistance value of a resistor with a given colors.
func Value(colors []string) int {
    band1 := resistorcolor.ColorCode(colors[0])
    band2 := resistorcolor.ColorCode(colors[1])
	return 10 * band1 + band2
}
