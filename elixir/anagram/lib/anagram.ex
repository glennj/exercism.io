defmodule Anagram do
  @doc """
  Returns all candidates that are anagrams of, but not equal to, 'base'.
  """
  @spec match(String.t(), [String.t()]) :: [String.t()]
  def match(base, candidates) do
    base_u = String.upcase(base)
    base_hash = hash(base)

    candidates
    |> Enum.filter(fn word -> base_u != String.upcase(word) end)
    |> Enum.filter(fn word -> base_hash == hash(word) end)
  end

  defp hash(word) do
    word
    |> String.upcase()
    |> String.to_charlist()
    |> Enum.sort()
  end
end
