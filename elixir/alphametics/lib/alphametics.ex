defmodule Alphametics do
  import Permutations

  @type puzzle :: binary
  @type solution :: %{required(?A..?Z) => 0..9}

  @doc """
  Takes an alphametics puzzle and returns a solution where every letter
  replaced by its number will make a valid equation. Returns `nil` when
  there is no valid solution to the given puzzle.
  """
  @spec solve(puzzle) :: solution | nil

  # a brute-force solution
  def solve(puzzle) do
    words =
      Regex.scan(~r/[[:alpha:]]+/, puzzle)
      |> Enum.concat()
      |> Enum.map(&to_charlist/1)

    letters = words |> Enum.concat() |> Enum.uniq()

    do_solve(
      permutations(Enum.to_list(0..9), length(letters)),
      words,
      letters
    )
  end

  @spec do_solve(list(list(integer)), list(charlist), charlist) :: solution | nil
  defp do_solve([], _, _), do: nil

  defp do_solve([perm | perms], words, letters) do
    map = Enum.zip(letters, perm) |> Map.new()
    nums = Enum.map(words, &word_to_num(&1, map))

    lhs = Enum.take(nums, length(words) - 1) |> Enum.sum()
    rhs = List.last(nums)

    any_leading_zeroes =
      for [h | _] <- words, reduce: false do
        acc -> acc || map[h] == 0
      end

    if lhs == rhs && not any_leading_zeroes do
      map
    else
      do_solve(perms, words, letters)
    end
  end

  @spec word_to_num(charlist, solution) :: non_neg_integer
  defp word_to_num(word, map),
    do: Enum.reduce(word, 0, &(&2 * 10 + map[&1]))
end
