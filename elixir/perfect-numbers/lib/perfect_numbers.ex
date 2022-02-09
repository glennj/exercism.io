defmodule PerfectNumbers do
  @doc """
  Determine the aliquot sum of the given `number`, by summing all the factors
  of `number`, aside from `number` itself.

  Based on this sum, classify the number as:

  :perfect if the aliquot sum is equal to `number`
  :abundant if the aliquot sum is greater than `number`
  :deficient if the aliquot sum is less than `number`
  """
  @spec classify(number :: integer) :: {:ok, atom} | {:error, String.t()}
  def classify(n) when n < 1 do
    {:error, "Classification is only possible for natural numbers."}
  end

  def classify(n) do
    do_classify(n, aliquot_sum(n))
  end

  defp do_classify(n, n), do: {:ok, :perfect}
  defp do_classify(n, sum) when sum < n, do: {:ok, :deficient}
  defp do_classify(n, sum) when sum > n, do: {:ok, :abundant}

  defp aliquot_sum(n) do
    do_aliquot_sum(n, 1, MapSet.new())
  end

  defp do_aliquot_sum(n, i, factors) when i * i > n do
    factors
    |> MapSet.delete(n)
    |> Enum.sum()
  end

  defp do_aliquot_sum(n, i, factors) when rem(n, i) == 0 do
    do_aliquot_sum(
      n,
      i + 1,
      factors |> MapSet.put(i) |> MapSet.put(div(n, i))
    )
  end

  defp do_aliquot_sum(n, i, factors) do
    do_aliquot_sum(n, i + 1, factors)
  end
end
