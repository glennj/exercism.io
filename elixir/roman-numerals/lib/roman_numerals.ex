defmodule RomanNumerals do
  @map [
    M: 1000, CM: 900, D: 500, CD: 400,
    C: 100,  XC: 90,  L: 50,  XL: 40,
    X: 10,   IX: 9,   V: 5,   IV: 4,
    I: 1
  ]

  @doc """
  Convert the number to a roman number.
  """
  @spec numeral(pos_integer) :: String.t()
  def numeral(n), do: do_roman(@map, n, "")

  defp do_roman(_, 0, digits), do: digits

  defp do_roman([{r, d} | map], n, digits) when n >= d do
    do_roman([{r, d} | map], n - d, digits <> to_string(r))
  end

  defp do_roman([_ | map], n, digits) do
    do_roman(map, n, digits)
  end
end
