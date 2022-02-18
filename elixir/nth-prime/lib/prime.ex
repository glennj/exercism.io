defmodule Prime do
  require Integer

  @doc """
  Generates the nth prime.
  """
  @spec nth(non_neg_integer) :: non_neg_integer
  def nth(count) when count < 1, do: raise ArgumentError

  def nth(count) do
    Stream.iterate(2, &next_prime_after/1)
    |> Enum.at(count - 1)
  end

  defp next_prime_after(2), do: 3

  defp next_prime_after(n) do
    n = n + 2
    if prime?(n), do: n, else: next_prime_after(n)
  end

  defp prime?(n) when n < 2, do: false
  defp prime?(2), do: true
  defp prime?(n) when Integer.is_even(n), do: false
  defp prime?(n), do: do_prime?(n, 3)

  defp do_prime?(n, i) when i * i > n, do: true
  defp do_prime?(n, i) when rem(n, i) == 0, do: false
  defp do_prime?(n, i), do: do_prime?(n, i + 2)
end
