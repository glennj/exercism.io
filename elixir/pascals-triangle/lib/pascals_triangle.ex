defmodule PascalsTriangle do
  @doc """
  Calculates the rows of a pascal triangle
  with the given height
  """
  @spec rows(integer) :: [[integer]]
  def rows(num) do
    do_pascal(num, num, [])
  end

  defp do_pascal(_, 0, result), do: result

  defp do_pascal(num, n, result) do
    do_pascal(num, n - 1, [row(n) | result])
  end

  defp row(n) do
    for k <- 1..n do choose(n - 1, k - 1) end
  end

  # Note: caching the factorial values, although adding complexity,
  # _might_ increase performance. TBD

  defp choose(n, k) do
    div(fact(n), fact(k) * fact(n - k))
  end

  defp fact(0), do: 1
  defp fact(x), do: Enum.reduce(1..x, 1, &*/2)
end
