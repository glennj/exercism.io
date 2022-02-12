defmodule Grains do
  use Bitwise

  @doc """
  Calculate two to the power of the input minus one.
  """
  @spec square(pos_integer()) :: {:ok, pos_integer()} | {:error, String.t()}
  def square(number) when number not in 1..64 do
    {:error, "The requested square must be between 1 and 64 (inclusive)"}
  end

  def square(number) do
    {:ok, 1 <<< (number - 1)}
  end

  @doc """
  Adds square of each number from 1 to 64.
  """
  @spec total :: {:ok, pos_integer()}
  def total do
    {:ok, (1 <<< 64) - 1}
  end
end
