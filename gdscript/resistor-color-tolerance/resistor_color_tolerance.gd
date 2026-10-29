# can't really reuse much: the requirements differ
var current_path = get_script().resource_path .get_base_dir()
var ResistorColor = load(current_path.path_join("resistor_color.gd"))
var rc = ResistorColor.new()


# ought to come from resistor-color-duo
func value(colors):
	var accumulate = func (sum, digit): return 10 * sum + digit
	return colors.map(rc.color_code).reduce(accumulate, 0)


# ought to come from resistor-color-trio
func label(colors):
	var first = colors.slice(0, -1)
	var last = colors[-1]
	var value = value(first) * 10 ** rc.color_code(last)
	return to_label(value)


func to_label(value):
	var i = 0
	while value >= 1000:
		i += 1
		value /= 1000.0
	return "{0} {1}ohms" \
			.format([value, ["", "kilo", "mega", "giga"][i]]) \
			.replace(".0 ", " ")


# ------------------------------------------------------------

const TOLERANCES = {
	"grey": 0.05,
	"violet": 0.1,
	"blue": 0.25,
	"green": 0.5,
	"brown": 1,
	"red": 2,
	"gold": 5,
	"silver": 10,
}


func label_decoder(colors):
	if colors.size() == 1:
		return "0 ohms"

	var label = label(colors.slice(0, -1))
	var tolerance = TOLERANCES[colors[-1]]

	return "{0} ±{1}%".format([label, tolerance])
