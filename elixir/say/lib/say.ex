defmodule Say do
  @ones {
    "zero", "one", "two", "three", "four", "five", "six", "seven",
    "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
    "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"
  }

  # values will be retrieved by index, hence the nils
  @tens {
    nil, nil, "twenty", "thirty", "forty", "fifty",
    "sixty", "seventy", "eighty", "ninety"
  }

  @doc """
  Translate a positive integer into English.
  """
  @spec in_english(integer) :: {atom, String.t()}
  def in_english(number) when number < 0 or number > 999_999_999_999 do
    {:error, "number is out of range"}
  end

  def in_english(n) do
    {:ok,
      cond do
        n < 100           -> small(n)
        n < 1000          -> compound(n, "hundred", 100)
        n < 1_000_000     -> compound(n, "thousand", 1000)
        n < 1_000_000_000 -> compound(n, "million", 1_000_000)
        true              -> compound(n, "billion", 1_000_000_000)
      end
    }
  end

  defp small(n) when n < 20, do: elem(@ones, n)

  defp small(n), do: do_small(divmod(n, 10))

  defp do_small({div, 0}) do
    elem(@tens, div)
  end

  defp do_small({div, rem}) do 
    elem(@tens, div) <> "-" <> elem(@ones, rem)
  end

  defp compound(n, unit, base), do: do_compound(divmod(n, base), unit)

  defp do_compound({div, 0}, unit) do
    "#{elem(in_english(div), 1)} #{unit}"
  end

  defp do_compound({div, rem}, unit) do
    "#{elem(in_english(div), 1)} #{unit} #{elem(in_english(rem), 1)}"
  end

  defp divmod(num, div), do: {div(num, div), rem(num, div)}
end
