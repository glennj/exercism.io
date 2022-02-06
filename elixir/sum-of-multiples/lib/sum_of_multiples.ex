defmodule SumOfMultiples do
  @doc """
  Adds up all numbers from 1 to a given end number that are multiples of the factors provided.
  """
  @spec to(non_neg_integer, [non_neg_integer]) :: non_neg_integer
  def to(limit, factors) do
    do_to(limit, factors, MapSet.new())
  end

  defp do_to(limit, [], set), do: MapSet.delete(set, limit) |> Enum.reduce(0, &+/2)

  defp do_to(limit, [0 | factors], set), do: do_to(limit, factors, set)

  defp do_to(limit, [f | factors], set) do
    do_to(
      limit,
      factors,
      f..limit//f |> Enum.reduce(set, fn mult, s -> MapSet.put(s, mult) end)
    )
  end
end
