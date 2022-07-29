defmodule Knapsack do
  @moduledoc """
  Translating the pseudocode from https://en.wikipedia.org/wiki/Knapsack_problem

    Assume w_{1}, w_{2}, ..., w_{n}, W are strictly positive
    integers.  Define m[i,w] to be the maximum value that can
    be attained with weight less than or equal to w using
    items up to i (first i items).
    
    We can define m[i,w] recursively as follows: 
    * m[0,w] = 0
    * m[i,w] = m[i-1,w] if w_{i} > w (the new item is more
      than the current weight limit)
    * m[i,w] = max(m[i-1,w], m[i-1,w-w_{i}]+v_{i}) if w_{i} <= w.

  The solution can then be found by extracting m[n,W]. 
  """

  @type item :: %{value: integer, weight: integer}
  @type table :: Map.t({integer, integer}, integer)

  @doc """
  Return the maximum value that a knapsack can carry.
  """
  @spec maximum_value([item], integer) :: integer
  def maximum_value(items, maximum_weight) do
    n = length(items)

    # set the maximum value for each weight when there are no items
    # then populate the table for max_value for each item and weight
    m =
      Map.new()
      |> row_zero(0, maximum_weight)
      |> max_value(items, 1, n, maximum_weight)

    Map.get(m, {n, maximum_weight})
  end

  # Generate the "zero'th" row of the result matrix.
  # This row seeds the rest of the table (that will have `n` more rows, 
  # one for each item)
  @spec row_zero(table, integer, integer) :: table
  defp row_zero(m, w, max_wt) when w > max_wt, do: m

  defp row_zero(m, w, max_wt) do
    row_zero(Map.put(m, {0, w}, 0), w + 1, max_wt)
  end

  # Iterate over the items to complete the result matrix.
  @spec max_value(table, [item], integer, integer, integer) :: table
  defp max_value(m, _, i, n, _) when i > n, do: m

  defp max_value(m, [item | items], i, n, max_wt) do
    m = row_i(m, item, i, n, 0, max_wt)
    max_value(m, items, i + 1, n, max_wt)
  end

  # Generate the "i'th" row of the result matrix.
  @spec row_i(table, [item], integer, integer, integer, integer) :: table
  defp row_i(m, _, _, _, w, max_wt) when w > max_wt, do: m

  defp row_i(m, item, i, n, w, max_wt) do
    m =
      Map.put(
        m,
        {i, w},
        if item[:weight] > w do
          Map.get(m, {i - 1, w})
        else
          max_of(
            Map.get(m, {i - 1, w}),
            item[:value] + Map.get(m, {i - 1, w - item[:weight]})
          )
        end
      )

    row_i(m, item, i, n, w + 1, max_wt)
  end

  @spec max_of(integer, integer) :: integer
  defp max_of(a, b) when a >= b, do: a
  defp max_of(_, b), do: b
end
