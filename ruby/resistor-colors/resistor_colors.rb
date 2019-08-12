require_relative "../resistor_color"

module ResistorColors
  module_function

  def value(colors)
    10 * color_code(colors[0]) + color_code(colors[1])
  end
end
