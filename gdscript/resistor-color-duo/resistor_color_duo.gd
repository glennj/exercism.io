var current_path = get_script().resource_path.get_base_dir()
var ResistorColor = load(current_path.path_join("resistor_color.gd"))
var rc = ResistorColor.new()

# ------------------------------------------------------------
# resistor-color-duo

func value(colors):
	return colors \
			.slice(0, 2) \
			.map(rc.color_code) \
			.reduce(func (val, code): return 10 * val + code, 0)
