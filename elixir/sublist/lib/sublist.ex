defmodule Sublist do
  @doc """
  Returns whether the first list is a sublist or a superlist of the second list
  and if not whether it is equal or unequal to the second list.
  """
  @spec compare(a :: [integer], b :: [integer]) :: boolean
  def compare(a, b) do
    case [length(a), length(b)] do
      [n, n] ->
        if starts_with?(a, b),
          do: :equal,
          else: :unequal

      [m, n] when m > n ->
        if contains?(a, b, m, n),
          do: :superlist,
          else: :unequal

      [m, n] ->
        # note the order of the parameters
        if contains?(b, a, n, m),
          do: :sublist,
          else: :unequal
    end
  end

  # the length of `a` will be >= length of `b`
  defp starts_with?([], []), do: true
  defp starts_with?([], _), do: false
  defp starts_with?(_, []), do: true
  defp starts_with?([ha | _], [hb | _]) when ha !== hb, do: false
  defp starts_with?([_ | ta], [_ | tb]), do: starts_with?(ta, tb)

  # contains?: iterate through the longer list, checking if
  # if starts with the shorter list at each iteration
  #
  defp contains?(a, b, la, lb) when la >= lb do
    case starts_with?(a, b) do
      true -> true
      false -> contains?(tl(a), b, la - 1, lb)
    end
  end

  defp contains?(_, _, _, _), do: false
end
