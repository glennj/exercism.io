package resistorcolor

var colors = []string{
	"black",
	"brown",
	"red",
	"orange",
	"yellow",
	"green",
	"blue",
	"violet",
	"grey",
	"white",
}

// Colors should return the list of all colors.
func Colors() []string {
	return colors
}

// ColorCode returns the resistance value of the given color.
func ColorCode(color string) int {
	switch color {
	case "black": return 0
	case "brown": return 1
	case "red": return 2
	case "orange": return 3
	case "yellow": return 4
	case "green": return 5
	case "blue": return 6
	case "violet": return 7
	case "grey": return 8
	case "white": return 9
	default: panic("unknown resistor color")
	}
}
