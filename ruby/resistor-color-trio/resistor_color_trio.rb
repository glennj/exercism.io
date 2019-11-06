require_relative '../resistor-color/resistor_color'

class ResistorColorTrio
  private

  attr_reader :colors

  UNIT_PREFIXES = ['', 'kilo', 'mega', 'giga'].freeze

  public

  def initialize(colors)
    @colors = colors.take(3)  # ignore extra colors
  end

  def value
    @value ||= begin
      codes = colors.map do |color|
        ResistorColor.color_code(color) ||
          raise(ArgumentError, 'invalid color')
      end
      (10 * codes[0] + codes[1]) * 10**codes[2]
    end
  end

  def label
    @label ||= begin
      idx = 0
      val = value
      while val == val / 1000 * 1000
        val /= 1000
        idx += 1
      end
      "Resistor value: #{val} #{UNIT_PREFIXES[idx]}ohms"
    end
  end
end
