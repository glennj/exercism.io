# I speak from experience as a colourblind person: resistor colours suck.

module ResistorColors
  module_function

  def value(colors)
    colors.reduce(0) do |sum, c| 
      sum * 10 + COLOR_MAP[c.downcase.to_sym]
    end
  end

  private

  COLOR_MAP = {
    black: 0, brown: 1, red:    2, orange: 3, yellow: 4,
    green: 5, blue:  6, violet: 7, grey:   8, white:  9,
  }.freeze
end
