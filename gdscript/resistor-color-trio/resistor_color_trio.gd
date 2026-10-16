var current_path = get_script().resource_path .get_base_dir()

var ResistorColor = load(current_path.path_join("resistor_color.gd"))
var ResistorColorDuo = load(current_path.path_join("resistor_color_duo.gd"))

var rc = ResistorColor.new()
var rc2 = ResistorColorDuo.new()


func label(colors):
	var value = rc2.value(colors) * 10 ** rc.color_code(colors[2])
	return to_label(value)


func to_label(value):
	var i = 0
	while value > 0 and value % 1000 == 0:
		i += 1
		value /= 1000
	return "{0} {1}ohms".format([value, ["", "kilo", "mega", "giga"][i]])
