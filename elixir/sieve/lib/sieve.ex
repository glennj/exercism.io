defmodule Sieve do
  @doc """
  Generates a list of primes up to a given limit.
  """
  @spec primes_to(non_neg_integer) :: [non_neg_integer]
  def primes_to(limit) when limit < 2, do: []

  def primes_to(limit) do
    flags = for i <- 2..limit do {i, true} end |> Map.new()

    flags = sieve(2, limit, flags)

    Enum.filter(2..limit, &flags[&1])
  end

  defp sieve(i, limit, flags) when i * i > limit, do: flags

  defp sieve(2, limit, flags) do
    sieve(3, limit, mark_multiples_of(flags, limit, 2))
  end

  defp sieve(i, limit, flags) do
    if flags[i] do
      sieve(i + 2, limit, mark_multiples_of(flags, limit, i))
    else
      sieve(i + 2, limit, flags)
    end
  end

  defp mark_multiples_of(flags, limit, n) do
    step = if n == 2, do: 2, else: n * 2
    do_mark_multiples(flags, limit, n * n, step)
  end

  defp do_mark_multiples(flags, limit, i, _) when i > limit, do: flags

  defp do_mark_multiples(flags, limit, i, step) do
    do_mark_multiples(
      %{flags | i => false},
      limit,
      i + step,
      step
    )
  end
end
