defmodule PrimeFactors do
  @doc """
  Compute the prime factors for 'number'.

  The prime factors are prime numbers that when multiplied give the desired
  number.

  The prime factors of 'number' will be ordered lowest to highest.
  """
  @spec factors_for(pos_integer) :: [pos_integer]
  def factors_for(number) do
    do_factors(number, 2, [])
  end

  defp do_factors(n, f, acc) when rem(n, f) == 0 and f * f <= n do
    do_factors(div(n, f), f, [f | acc])
  end

  defp do_factors(n, f, acc) when f * f <= n do
    do_factors(n, f + 1, acc)
  end

  defp do_factors(n, f, acc) when n > 1 do
    do_factors(1, f, [n | acc])
  end

  defp do_factors(_, _, acc), do: Enum.reverse(acc)
end
