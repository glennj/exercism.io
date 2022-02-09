defmodule Squares do
  @spec sum_of_squares(pos_integer) :: pos_integer
  def sum_of_squares(number) do
    for(i <- 1..number, do: i ** 2) |> Enum.sum()
  end

  @spec square_of_sum(pos_integer) :: pos_integer
  def square_of_sum(number) do
    Enum.sum(1..number) ** 2
  end

  @spec difference(pos_integer) :: pos_integer
  def difference(number) do
    abs(square_of_sum(number) - sum_of_squares(number))
  end
end
