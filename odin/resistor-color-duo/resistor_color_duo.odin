package resistor_color_duo

Color :: enum {
	Black,
	Brown,
	Red,
	Orange,
	Yellow,
	Green,
	Blue,
	Violet,
	Grey,
	White,
}

Error :: enum {
	None,
	TooFewColors,
	Unimplemented,
}

value :: proc(colors: []Color) -> (int, Error) {
	if len(colors) < 2 {
		return 0, .TooFewColors
	}
	band1 := int(colors[0])
	band2 := int(colors[1])
	return 10 * band1 + band2, .None
}
