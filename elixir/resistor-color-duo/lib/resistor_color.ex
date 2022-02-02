defmodule ResistorColor do
  @colours [:black, :brown, :red, :orange, :yellow, :green, :blue, :violet, :grey, :white]

  @doc """
  Return the value of a color band
  """
  @spec code(atom) :: integer()
  def code(color), do: index_of(color, @colours)

  defp index_of(elem, [elem | _]), do: 0
  defp index_of(elem, [_ | tail]), do: 1 + index_of(elem, tail)
end
