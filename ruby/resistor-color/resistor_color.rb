module ResistorColor

  COLORS = %w[
    black brown red orange yellow green blue violet grey white
  ].freeze

  module_function

  def color_code(color)
    # No specification for "invalid color".
    # Here, I return `nil`
    COLORS.index(color)
  end
end
