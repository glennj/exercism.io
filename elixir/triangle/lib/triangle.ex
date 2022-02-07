defmodule Triangle do
  @type kind :: :equilateral | :isosceles | :scalene

  @doc """
  Return the kind of triangle of a triangle with 'a', 'b' and 'c' as lengths.
  """
  @spec kind(number, number, number) :: {:ok, kind} | {:error, String.t()}

  # inspired by https://exercism.org/tracks/elixir/exercises/triangle/solutions/jameslong 
  # a single case:
  #
  def kind(a, b, c) do
    case Enum.sort([a, b, c]) do
      [d, _, _] when d <= 0 -> {:error, "all side lengths must be positive"}
      [d, e, f] when d + e < f -> {:error, "side lengths violate triangle inequality"}

      [d, d, d] -> {:ok, :equilateral}
      [d, d, _] -> {:ok, :isosceles}
      [_, d, d] -> {:ok, :isosceles}
      [_, _, _] -> {:ok, :scalene}
    end
  end

  ## my first iteration, using multiple clause functions
  #
  # def kind(a, b, c) do
  #   kind([a, b, c], valid?(Enum.sort([a, b, c])))
  # end
  #
  # defp kind(sides, :ok) do 
  #   case MapSet.new(sides) |> MapSet.size() do
  #     1 -> {:ok, :equilateral}
  #     2 -> {:ok, :isosceles}
  #     3 -> {:ok, :scalene}
  #   end
  # end
  #
  # defp kind(_, {:error, _} = result), do: result
  #
  # # triangle inequality
  # defp valid?([a, _, c]) when a <= 0 or c == 0 do
  #   {:error, "all side lengths must be positive"}
  # end
  #
  # defp valid?([a, b, c]) when a + b < c do
  #   {:error, "side lengths violate triangle inequality"}
  # end
  #
  # defp valid?(_), do: :ok
end
