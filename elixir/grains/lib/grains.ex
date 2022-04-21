defmodule Grains do
  use Bitwise

  @bad_square "The requested square must be between 1 and 64 (inclusive)"

  @doc """
  Calculate two to the power of the input minus one.
  """
  @spec square(pos_integer()) :: {:ok, pos_integer()} | {:error, String.t()}
  def square(number) when number not in 1..64, do: {:error, @bad_square}
  def square(number), do: {:ok, 1 <<< (number - 1)}

  @doc """
  Adds square of each number from 1 to 64.
  """
  @spec total :: {:ok, pos_integer()}
  def total do
    ## implementation 1: direct formula
    # {:ok, (1 <<< 64) - 1}

    ## implementation 2: helper function
    # {:ok, do_total(1, 0)}

    # implementation 3: for loop
    sum = for i <- 1..64, {:ok, sq} = square(i), reduce: 0 do sum -> sum + sq end
    {:ok, sum}
  end

  # defp do_total(i, sum) when i > 64, do: sum
  #
  # defp do_total(i, sum) do
  #   {:ok, sq} = square(i)
  #   do_total(i + 1, sum + sq)
  # end
end
