defmodule WordCount do
  @moduledoc """
  Iteration 1:

  This is a little state machine: iterate over the graphemes
  of the sentence. 
  - in state :a, we're looking for a character that _starts_ a word
  - in state :n, we're looking for a character _following_ the end of a word

  ------------
  Iteration 2:

  A very compact functional solution.
  """

  @doc """
  Count the number of words in the sentence.
  """
  @spec count(String.t()) :: map
  def count(sentence) do
    case :two do
      :one ->
        # iteration 1: this is my state machine solution
        do_count(
          sentence |> String.downcase() |> String.graphemes(),
          :a,
          nil,
          %{}
        )

      :two ->
        # iteration 2: remember unicode option for regex
        word_re = ~r/[[:alnum:]][[:alnum:]'-]*/u

        Regex.scan(word_re, String.downcase(sentence))
        |> Enum.map(fn [word | _] -> Regex.replace(~r/['-]+$/, word, "") end)
        |> Enum.reduce(%{}, fn word, count -> Map.update(count, word, 1, &(&1 + 1)) end)
    end
  end

  # Iteration 1 requires the following...

  # end of the sentence base cases
  defp do_count([], :a, _, count), do: count
  defp do_count([], :n, word, count), do: add_word(count, word)

  defp do_count([c | cs], :a, _, count) do
    if String.match?(c, ~r/[[:alnum:]]/) do
      # we've found the first character of a word
      do_count(cs, :n, c, count)
    else
      # keep looking
      do_count(cs, :a, nil, count)
    end
  end

  defp do_count([c | cs], :n, word, count) do
    if String.match?(c, ~r/[[:alnum:]'-]/) do
      # still inside a word: append the character and continue
      do_count(cs, :n, word <> c, count)
    else
      # we've found the end of a word
      do_count(cs, :a, nil, add_word(count, word))
    end
  end

  defp add_word(count, word) do
    trimmed =
      word
      |> String.trim_trailing("'")
      |> String.trim_trailing("-")

    Map.update(count, trimmed, 1, &(&1 + 1))
  end
end
