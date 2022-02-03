defmodule SquareRoot do
  @moduledoc """
  Using the Binary numeral system implementation from [Wikipedia][wiki]

  [wiki]: https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Binary_numeral_system_(base_2)
  """

  @doc """
  Calculate the integer square root of a positive integer
  """
  @spec calculate(radicand :: pos_integer) :: pos_integer
  def calculate(radicand), do: sqrt(radicand, 0, get_b(radicand, 1))

  # Find b, the greatest power of 4 less then or equal to n
  defp get_b(n, b) when b <= n, do: get_b(n, b * 4)
  defp get_b(_, b), do: div(b, 4)

  # the algorithm
  defp sqrt(_, x, b) when b == 0, do: x

  defp sqrt(n, x, b) when n >= b + x do
    sqrt(n - x - b, div(x, 2) + b, div(b, 4))
  end

  defp sqrt(n, x, b) do
    sqrt(n, div(x, 2), div(b, 4))
  end
end
