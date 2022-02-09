defmodule BeerSong do
  @ob "of beer"
  @ow "on the wall"

  @doc """
  Get a single verse of the beer song
  """
  @spec verse(integer) :: String.t()
  def verse(n) do
    b = bottles(n)
    """
    #{String.capitalize(b)} #{@ob} #{@ow}, #{b} #{@ob}.
    #{action(n)}, #{bottles(n - 1)} #{@ob} #{@ow}.
    """
  end

  defp bottles(0), do: "no more bottles"
  defp bottles(1), do: "1 bottle"
  defp bottles(-1), do: bottles(99)
  defp bottles(n), do: "#{n} bottles"

  defp action(0), do: "Go to the store and buy some more"
  defp action(n), do: "Take #{one(n)} down and pass it around"

  defp one(1), do: "it"
  defp one(_), do: "one"

  @doc """
  Get the entire beer song for a given range of numbers of bottles.
  """
  @spec lyrics(Range.t()) :: String.t()
  def lyrics(range \\ 99..0) do
    Enum.map(range, &verse/1)
    |> Enum.join("\n")
  end
end
