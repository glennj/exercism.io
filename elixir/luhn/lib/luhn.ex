defmodule Luhn do
  # double the number and sum the digits
  @luhn_digit %{
    0 => 0, 1 => 2, 2 => 4, 3 => 6, 4 => 8,
    5 => 1, 6 => 3, 7 => 5, 8 => 7, 9 => 9
  }

  @doc """
  Checks if the given number is valid via the luhn formula
  """
  @spec valid?(String.t()) :: boolean
  def valid?(number) do
    case digits(number) do
      :non_digit -> false
      [_] -> false
      ds -> do_valid?(ds, 0, false)
    end
  end

  defp digits(number) do
    try do
      number
      |> String.replace(" ", "")
      |> String.graphemes()
      |> Enum.map(&String.to_integer/1)  # might raise
      |> Enum.reverse()
    rescue
      ArgumentError -> :non_digit
    end
  end

  defp do_valid?([], sum, _), do: rem(sum, 10) == 0

  defp do_valid?([d | ds], sum, double?) do
    do_valid?(
      ds,
      sum + if(double?, do: @luhn_digit[d], else: d),
      not double?
    )
  end
end
