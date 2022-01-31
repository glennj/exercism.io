require_relative './resistor_color_duo'

class ResistorColorTrio
  private

  attr_reader :colors

  UNIT_PREFIXES = ['', 'kilo', 'mega', 'giga'].freeze

  public

  def initialize(colors)
    @colors = colors.take(3)  # ignore extra colors
  end

  def value
    @value = ResistorColorDuo.value(colors.take(2))
    @value *= 10 ** ResistorColor.color_code(colors[2])
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
