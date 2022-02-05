defmodule AllYourBase do
  @doc """
  Given a number in input base, represented as a sequence of digits, converts it to output base,
  or returns an error tuple if either of the bases are less than 2
  """

  @spec convert(list, integer, integer) :: {:ok, list} | {:error, String.t()}
  def convert(_, _, obase) when obase < 2, do: {:error, "output base must be >= 2"}
  def convert(_, ibase, _) when ibase < 2, do: {:error, "input base must be >= 2"}

  def convert(digits, input_base, output_base) do
    case do_convert_to_decimal(digits, input_base, 0) do
      {:error, _} = err -> err
      {:ok, decimal_value} -> do_convert_to_obase(decimal_value, output_base, [])
    end
  end

  @spec do_convert_to_decimal(
          digits :: [integer],
          input_base :: integer,
          decimal_value :: integer
        ) :: {:ok, integer} | {:error, String.t()}

  defp do_convert_to_decimal([], _, dec), do: {:ok, dec}

  defp do_convert_to_decimal([d | _], ibase, _) when d not in 0..(ibase - 1) do
    {:error, "all digits must be >= 0 and < input base"}
  end

  defp do_convert_to_decimal([d | digits], ibase, dec) do
    do_convert_to_decimal(digits, ibase, ibase * dec + d)
  end

  @spec do_convert_to_obase(
          decimal_value :: integer,
          output_base :: integer,
          digits :: [integer]
        ) :: {atom, [integer]}

  defp do_convert_to_obase(0, _, []), do: {:ok, [0]}
  defp do_convert_to_obase(0, _, digits), do: {:ok, digits}

  defp do_convert_to_obase(dec, obase, digits) do
    do_convert_to_obase(div(dec, obase), obase, [rem(dec, obase) | digits])
  end
end
