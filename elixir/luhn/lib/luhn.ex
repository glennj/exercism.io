defmodule Luhn do
  @luhn_digit %{
    true  => %{0=>0, 1=>2, 2=>4, 3=>6, 4=>8, 5=>1, 6=>3, 7=>5, 8=>7, 9=>9},
    false => %{0=>0, 1=>1, 2=>2, 3=>3, 4=>4, 5=>5, 6=>6, 7=>7, 8=>8, 9=>9},
  }

  @doc """
  Checks if the given number is valid via the luhn formula
  """
  @spec valid?(String.t()) :: boolean
  def valid?(number) do
    case digits(number) do
      :non_digit -> false
      [_]        -> false  # single digit numbers are not valid
      ds         -> do_valid?(Enum.reverse(ds), 0, false)
    end
  end

  defp digits(number) do
    # this would be a good approach:
    #    case number |> String.replace(" ", "") |> Integer.parse(10) do
    #      {num, ""} -> Integer.digits(num)
    #      _ -> :non_digit
    #    end
    #
    # except that it collapses "0001" to [1], when we want [0, 0, 0, 1]

    try do
      number
      |> String.replace(" ", "")
      |> String.graphemes()
      |> Enum.map(&String.to_integer/1)  # might raise
    rescue
      ArgumentError -> :non_digit
    end
  end

  defp do_valid?([], sum, _), do: rem(sum, 10) == 0

  defp do_valid?([d | ds], sum, double?) do
    value = @luhn_digit[double?][d]
    do_valid?(ds, sum + value, not double?)
  end
end
