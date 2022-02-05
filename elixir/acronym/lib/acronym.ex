defmodule Acronym do
  @doc """
  Generate an acronym from a string.
  "This is a string" => "TIAS"
  """
  @spec abbreviate(String.t()) :: String.t()
  def abbreviate(string) do
    do_abbr(String.upcase(string) |> String.to_charlist(), :alpha, '')
  end

  # This is implemented with in little state machine.
  # Iterating over the string character-by-character:
  #
  # - in `:alpha` state, we skip over characters until we find a letter, then:
  #     - add the letter to the acronym
  #     - change the state to `:nonalpha`
  # - in `:nonalpha` state, we skip over characters that _are_ letters until
  #   we find a "non-alpha" (apostrophe are considered letters in this state)
  #     - change the state to `:alpha`

  @type state :: :alpha | :nonalpha

  @spec do_abbr(chars :: charlist, state :: state, abbr :: charlist) :: String.t()

  defp do_abbr([], _, abbr), do: to_string(abbr)

  defp do_abbr([char | chars], :alpha, abbr) when char in ?A..?Z do
    do_abbr(chars, :nonalpha, abbr ++ [char])
  end

  # apostrophes are to be ignored
  defp do_abbr([?' | chars], :nonalpha, abbr) do
    do_abbr(chars, :nonalpha, abbr)
  end

  defp do_abbr([char | chars], :nonalpha, abbr) when char not in ?A..?Z do
    do_abbr(chars, :alpha, abbr)
  end

  defp do_abbr([_ | chars], state, abbr), do: do_abbr(chars, state, abbr)
end
