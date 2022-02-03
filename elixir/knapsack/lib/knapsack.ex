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
 
  The solution can then be found by calculating m[n,W]. 
  """

  @doc """
  Return the maximum value that a knapsack can carry.
  """
  @spec maximum_value(items :: [%{value: integer, weight: integer}], maximum_weight :: integer) ::
          integer
  def maximum_value(items, maximum_weight) do
    n = length(items)
    m = Map.new()

    # set the maximum value for each weight when there are no
    # items
    m = map_put(m, 0, maximum_weight, 0
    for w <- 0..maximum_weight, do: Map.put(m, {0, w}, 0)
      IO.inspect(m, label: "initial"):x


    m = max_value(m, items, 1, n, maximum_weight)

    Map.get(m, {n, maximum_weight})
  end

  defp max_value(m, [item | items], i, n, max_wt) when i <= n do
    IO.inspect(m, label: "before")
    for w <- 0..max_wt do
      Map.put(m, {i-1, w},
        case item[:weight] > w do
          true -> Map.get(m, {i-1, w})
          false ->
            max_of(
              Map.get(m, {i-1, w}),
              item[:value] + Map.get(m, {i-1, w - item[:weight]})
            )
        end
      )
    end
    IO.inspect(m, label: "after")
    max_value(m, items, i+1, n, max_wt)
  end

  defp max_value(m, _, _, _, _), do: m

  @spec max_of(a :: integer, b :: integer) :: integer
  defp max_of(a, b) when a >= b, do: a
  defp max_of(_, b), do: b
end
