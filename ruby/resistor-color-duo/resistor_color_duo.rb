require_relative "../resistor-color/resistor_color"

module ResistorColorDuo
  module_function

  def value(colors)
    colors.reduce(0) do |val, color|
      val * 10 + ResistorColor.color_code(color)
    end
  end
end
