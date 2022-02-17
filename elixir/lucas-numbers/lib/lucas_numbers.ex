defmodule LucasNumbers do
  @moduledoc """
  Lucas numbers are an infinite sequence of numbers which build progressively
  which hold a strong correlation to the golden ratio (φ or ϕ)

  E.g.: 2, 1, 3, 4, 7, 11, 18, 29, ...
  """

  def generate(count) when not is_integer(count) or count < 1 do
    raise ArgumentError, "count must be specified as an integer >= 1"
  end
  
  def generate(1), do: [2]
  def generate(2), do: [2, 1]

  def generate(count) do
    generate(2)
    |> Stream.iterate(fn [a, b] -> [b, a + b] end)
    |> Stream.map(&hd/1)
    |> Enum.take(count)
  end
end
