defmodule Raindrops do
  @drops [Pling: 3, Plang: 5, Plong: 7]

  @doc """
  Returns a string based on raindrop factors.
  """
  @spec convert(pos_integer) :: String.t()
  def convert(n) do
    case Enum.filter(@drops, fn {_, m} -> rem(n, m) == 0 end) do
      [] ->
        to_string(n)

      drops ->
        drops
        |> Enum.map(fn {d, _} -> to_string(d) end)
        |> Enum.join("")
    end
  end
end
