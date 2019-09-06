require_relative "../resistor-color/resistor_color"

class ResistorColorTrio
  def initialize(colors)
    @colors = colors.take(3)  # ignore extra colors
    @value = nil
    @label = nil
  end

  def value
    if @value.nil?
      codes = @colors.map do |color|
        code = ResistorColor.color_code(color)
        raise ArgumentError, "invalid color" if code.nil?
        code
      end
      @value = (10 * codes[0] + codes[1]) * 10 ** codes[2]
    end
    @value
  end

  def label
    @label ||= begin
      idx = 0
      val = value
      while val == val / 1000 * 1000
        val /= 1000
        idx += 1
      end

      sprintf("Resistor value: %d %sohms",
        val,
        ["", "kilo", "mega", "giga"][idx]
      )
    end
  end
end
