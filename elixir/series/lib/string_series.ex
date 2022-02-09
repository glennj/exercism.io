defmodule StringSeries do
  @doc """
  Given a string `s` and a positive integer `size`, return all substrings
  of that size. If `size` is greater than the length of `s`, or less than 1,
  return an empty list.
  """
  @spec slices(s :: String.t(), size :: integer) :: list(String.t())
  def slices(_, size) when size < 1, do: []

  def slices(s, size) do
    len = String.length(s)

    ## with `for` it's very straightforward
    # for i <- 0..(len - size),
    #     size in 1..len,
    #     do: String.slice(s, i, size)

    ## let's try it recursively
    get_slices(String.graphemes(s), len, size, [])
  end

  defp get_slices(_, len, size, slices) when len < size do
    slices |> Enum.reverse()
  end

  defp get_slices([_ | t] = chars, len, size, slices) do
    get_slices(
      t,
      len - 1,
      size,
      [get_one_slice(chars, size, 0, []) | slices]
    )
  end

  defp get_one_slice(_, size, i, slice) when i == size do
    slice |> Enum.reverse() |> Enum.join()
  end

  defp get_one_slice([c | chars], size, i, slice) do
    get_one_slice(chars, size, i + 1, [c | slice])
  end
end
