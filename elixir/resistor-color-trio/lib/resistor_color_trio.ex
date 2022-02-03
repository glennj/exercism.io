defmodule ResistorColorTrio do
  require ResistorColor
  require ResistorColorDuo

  @doc """
  Calculate the resistance value in ohm or kiloohm from resistor colors
  """
  @spec label(colors :: [atom]) :: {number, :ohms | :kiloohms}
  def label(colors) do
    [c1, c2, c3] = Enum.take(colors, 3)
    value = ResistorColorDuo.value([c1, c2]) * 10 ** ResistorColor.code(c3)

    case rem(value, 1000) do
      0 -> {div(value, 1000), :kiloohms}
      _ -> {value, :ohms}
    end
  end
end
