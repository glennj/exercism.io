module ResistorColor

  COLORS = %w[
    black brown red orange yellow green blue violet grey white
  ].freeze

  module_function

  def color_code(color)
    raise ArgumentError, 'invalid color' unless COLORS.include? color
    COLORS.index(color)
  end
end
