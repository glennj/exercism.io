defmodule ArmstrongNumber do
  @moduledoc """
  Provides a way to validate whether or not a number is an Armstrong number
  """

  @spec valid?(integer) :: boolean
  def valid?(number), do: number === armstrong_sum(number)

  defp armstrong_sum(number) do
    {len, digits} = digits_of(number, {0, []})

    Enum.reduce(digits, 0, fn d, sum -> sum + d ** len end)
  end

  # Extract the decimal digits of an integer.
  # For efficiency, determine the number of digits at the same time.

  @spec digits_of(
          number :: integer,
          {length :: integer, digits :: [integer]}
        ) :: {integer, [integer]}

  defp digits_of(0, result), do: result

  defp digits_of(n, {len, digits}) do
    digits_of(div(n, 10), {len + 1, [rem(n, 10) | digits]})
  end
end
