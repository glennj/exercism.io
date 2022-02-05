defmodule PigLatin do

  # storing the regexes in a keyword list to maintain the order
  @regexes [
    apple: ~r/^(?<prefix>)(?<rest>(?:[aeiou]|[xy][^aeiou]).*)/,
    rhythm: ~r/^(?<prefix>[^aeiou]+)(?<rest>y.+)/,
    queen: ~r/^(?<prefix>[^aeiou]*qu)(?<rest>.+)/,
    school: ~r/^(?<prefix>[^aeiou]+)(?<rest>.+)/
  ]

  @doc """
  Given a `phrase`, translate it a word at a time to Pig Latin.
  """
  @spec translate(phrase :: String.t()) :: String.t()
  def translate(phrase) do
    phrase
    |> String.split(" ")
    |> Enum.map(fn word -> do_translate_word(word, @regexes) end)
    |> Enum.join(" ")
  end

  @spec do_translate_word(
          word :: String.t(),
          regexes :: list(keyword(Regex.t()))
        ) :: String.t()

  # fallback if none of the patterns match
  defp do_translate_word(word, []), do: word <> "ay"

  defp do_translate_word(word, [{_, re} | regexes]) do
    if m = Regex.named_captures(re, word) do
      m["rest"] <> m["prefix"] <> "ay"
    else
      do_translate_word(word, regexes)
    end
  end
end
