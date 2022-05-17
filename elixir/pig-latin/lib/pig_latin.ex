defmodule PigLatin do
  @doc """
  Given a `phrase`, translate it a word at a time to Pig Latin.
  """
  @spec translate(phrase :: String.t()) :: String.t()
  def translate(phrase) do
    phrase
    |> String.split("\s")
    |> Enum.map_join(" ", &translate_word/1)
  end

  @regexes [
    # word starts with a vowel, or "x" or "y" followed by non-vowels
    # example: apple => appleay; xray => xrayay
    ~r/^ (?<prefix>) (?<rest> (?: [aeiou] | [xy][^aeiou]) .*) /x,

    # starts with non-vowels followed by a "y"
    # example: rhythm => ythmrhay
    ~r/^ (?<prefix> [^aeiou]+) (?<rest> y.*) /x,

    # starts with optional non-vowels then "qu"
    # example: square => aresquay
    ~r/^ (?<prefix> [^aeiou]* qu) (?<rest> .+) /x,

    # starts with non-vowel(s)
    # example: school => oolschay
    ~r/^ (?<prefix> [^aeiou]+) (?<rest> .*) /x
  ]

  defp translate_word(word, regexes \\ @regexes)

  # fallback if none of the patterns match
  defp translate_word(word, []), do: pigify(word)

  defp translate_word(word, [regex | regexes]) do
    case Regex.named_captures(regex, word) do
      nil -> translate_word(word, regexes)
      m -> pigify(m["prefix"], m["rest"])
    end
  end

  defp pigify(prefix \\ "", middle)
  defp pigify(prefix, middle), do: "#{middle}#{prefix}ay"
end
