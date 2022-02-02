defmodule ResistorColorDuo do
  require ResistorColor

  @doc """
  Calculate a resistance value from two colors
  """
  @spec value(colors :: [atom]) :: integer
  def value(colors) do
    [c1 | rest] = colors
    [c2 | _] = rest
    10 * ResistorColor.code(c1) + ResistorColor.code(c2)
  end
end
