defmodule KillerSudokuHelper do
  # an unfortunate name collision. Shame `import` can't alias function names.
  import Permutations, only: [combinations: 2]

  @type cage_t :: %{exclude: list(integer), size: integer, sum: integer}

  @doc """
  Return the possible combinations of `size` distinct numbers from 1-9 excluding `exclude` that sum up to `sum`.
  """
  @spec combinations(cage_t()) :: list(list(integer))
  def combinations(cage) do
    digits = [1, 2, 3, 4, 5, 6, 7, 8, 9] -- cage.exclude

    for combo <- combinations(digits, cage.size),
        Enum.sum(combo) == cage.sum,
        do: combo
  end
end
