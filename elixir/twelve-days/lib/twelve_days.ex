defmodule TwelveDays do
  @gifts {
    "a Partridge in a Pear Tree", "two Turtle Doves",
    "three French Hens", "four Calling Birds",
    "five Gold Rings", "six Geese-a-Laying",
    "seven Swans-a-Swimming", "eight Maids-a-Milking",
    "nine Ladies Dancing", "ten Lords-a-Leaping",
    "eleven Pipers Piping", "twelve Drummers Drumming"
  }

  @nth {
    "first", "second", "third", "fourth", "fifth", "sixth",
    "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth"
  }

  @doc """
  Given a `number`, return the song's verse for that specific day, including
  all gifts for previous days in the same line.
  """
  @spec verse(number :: integer) :: String.t()
  def verse(n) do
    "On the #{nth(n)} day of Christmas my true love gave to me: #{gifts(n, 1, [])}."
  end

  defp gifts(n, m, gifts) when m > n, do: Enum.join(gifts, ", ")
  defp gifts(n, 1, gifts) when n > 1, do: gifts(n, 2, ["and #{gift(1)}" | gifts])
  defp gifts(n, i, gifts), do: gifts(n, i + 1, [gift(i) | gifts])

  defp gift(i), do: elem(@gifts, i - 1)

  defp nth(i), do: elem(@nth, i - 1)

  @doc """
  Given a `starting_verse` and an `ending_verse`, return the verses for each
  included day, one per line.
  """
  @spec verses(starting_verse :: integer, ending_verse :: integer) :: String.t()
  def verses(starting_verse, ending_verse) do
    for i <- starting_verse..ending_verse do
      verse(i)
    end
    |> Enum.join("\n")
  end

  @doc """
  Sing all 12 verses, in order, one verse per line.
  """
  @spec sing() :: String.t()
  def sing, do: verses(1, 12)
end
