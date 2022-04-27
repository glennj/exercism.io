defmodule Say do
  @ones {
    "zero", "one", "two", "three", "four", "five", "six", "seven",
    "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
    "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"
  }

  # values will be retrieved by index (9 => "ninety"), hence the nils
  @tens {
    nil, nil, "twenty", "thirty", "forty", "fifty",
    "sixty", "seventy", "eighty", "ninety"
  }

  @doc """
  Translate a positive integer into English.
  """
  @spec in_english(integer) :: {:ok | :error, String.t()}
  def in_english(number) when number < 0 or number > 999_999_999_999,
    do: {:error, "number is out of range"}

  def in_english(n), do: {:ok, say(n)}

  defp divmod(number, divisor), do: {div(number, divisor), rem(number, divisor)}

  defp say(n) when n < 100,           do: small(n)
  defp say(n) when n < 1000,          do: compound(divmod(n, 100), "hundred")
  defp say(n) when n < 1_000_000,     do: compound(divmod(n, 1000), "thousand")
  defp say(n) when n < 1_000_000_000, do: compound(divmod(n, 1_000_000), "million")
  defp say(n),                        do: compound(divmod(n, 1_000_000_000), "billion")

  defp small(n) when n < 20, do: elem(@ones, n)
  defp small(n) do
    case divmod(n, 10) do
      {tens,    0} -> elem(@tens, tens)
      {tens, ones} -> "#{elem(@tens, tens)}-#{elem(@ones, ones)}"
    end
  end

  defp compound({quo,   0}, unit), do: "#{say(quo)} #{unit}"
  defp compound({quo, rem}, unit), do: "#{say(quo)} #{unit} #{say(rem)}"
end
