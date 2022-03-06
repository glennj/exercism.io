defmodule Permutations do
  @moduledoc """
  Combinations and permutations

  From:
  - https://www.erlang.org/doc/programming_examples/list_comprehensions.html#permutations
  - https://rosettacode.org/wiki/Combinations
  """

  @doc """
  Generate all permutations of a list of items.

  E.g.
    
      > permutations([1, 2, 3])
      [[1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1]]
  """
  def permutations([]), do: [[]]

  def permutations(list) do
    for h <- list, t <- permutations(list -- [h]), do: [h | t]
  end

  @doc """
  Generate all (ordered) combinations of _k_ items from a list of _n_ items.
  This is "n choose k".

  E.g.
    
      > combinationns([1, 2, 3, 4], 3)
      [[1, 2, 3], [1, 2, 4], [1, 3, 4], [2, 3, 4]]
  """
  def combinations(_list, 0), do: [[]]
  def combinations([], _size), do: []

  def combinations([h | t], size) do
    h_combs = for l <- combinations(t, size - 1), do: [h | l]
    h_combs ++ combinations(t, size)
  end

  @doc """
  Generate all permutations of _k_ items from a list of _n_ items.

  E.g.

      > Permutations.permutations([1, 2, 3, 4], 3)
      [ [1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1],
        [1, 2, 4], [1, 4, 2], [2, 1, 4], [2, 4, 1], [4, 1, 2], [4, 2, 1],
        [1, 3, 4], [1, 4, 3], [3, 1, 4], [3, 4, 1], [4, 1, 3], [4, 3, 1],
        [2, 3, 4], [2, 4, 3], [3, 2, 4], [3, 4, 2], [4, 2, 3], [4, 3, 2]
      ]
  """
  def permutations(list, size) do
    combinations(list, size)
    |> Enum.flat_map(&permutations/1)
  end

end
