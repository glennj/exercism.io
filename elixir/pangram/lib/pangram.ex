defmodule Pangram do
  @alphabet ?A..?Z

  @doc """
  Determines if a word or sentence is a pangram.
  A pangram is a sentence using every letter of the alphabet at least once.

  Returns a boolean.
  """

  @spec pangram?(String.t()) :: boolean
  def pangram?(sentence) do
    Range.size(@alphabet) === sentence |> letters() |> MapSet.size()
  end

  @spec letters(String.t()) :: MapSet.t()
  defp letters(sentence) do
    sentence
    |> String.upcase()
    |> String.to_charlist()
    |> Enum.filter(fn c -> c in @alphabet end)
    |> MapSet.new()
  end
end
