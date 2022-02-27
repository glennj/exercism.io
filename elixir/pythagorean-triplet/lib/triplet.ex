defmodule Triplet do
  @doc """
  Calculates sum of a given triplet of integers.
  """
  @spec sum([non_neg_integer]) :: non_neg_integer
  defdelegate sum(list), to: Enum

  @doc """
  Calculates product of a given triplet of integers.
  """
  @spec product([non_neg_integer]) :: non_neg_integer
  defdelegate product(list), to: Enum

  @doc """
  Determines if a given triplet is pythagorean. That is, do the squares of a and b add up to the square of c?
  """
  @spec pythagorean?([non_neg_integer]) :: boolean
  def pythagorean?([a, b, c]), do: a * a + b * b == c * c

  @doc """
  Generates a list of pythagorean triplets whose values add up to a given sum.

  Brute force, slow for large sums.

  The filter `3 <= a and a < b` is a *lot* faster
  than `a in 3..(b - 1)` -- 4 sec versus 27 sec.
  I found the magnitude of the difference surprising, even
  if `in` has to do a bit more work:
  https://github.com/elixir-lang/elixir/blob/main/lib/elixir/lib/range.ex#L370

  """
  @spec generate(non_neg_integer) :: [list(non_neg_integer)]
  def generate(sum) do
    {b_min, c_min} = minimum_triangle_sides(sum)

    # `3` and `4` here are taken from the smallest pythagorean triplet,
    # so we can save a few loop iterations
    for c <- (sum - 3 - 4)..c_min//-1,
        b <- (c - 1)..b_min//-1,
        a = sum - c - b,
        3 <= a and a < b,
        pythagorean?([a, b, c]),
        do: [a, b, c]

    ## using streams is slow:
    # (sum - 3 - 4)..c_min//-1 
    # |> Stream.flat_map(fn c ->
    #   Stream.map((c - 1)..b_min//-1, fn b -> [sum - b - c, b, c] end)
    # end) 
    # |> Stream.filter(fn [a, b, _] -> 3 <= a and a < b end) 
    # |> Enum.filter(&pythagorean?/1)

  end

  defp minimum_triangle_sides(sum) do
    # the smallest side `b` such that a <= b is where the sides
    # adjacent to the right angle are equal. 
    # Then, c² = b² + b² ==> c = √2 b
    # and   sum = b + b + c = b × (2 + √2) ==> b = sum ÷ (2 + √2)
    b = (sum / (2 + :math.sqrt(2))) |> trunc()
    c = (:math.sqrt(2) * b) |> trunc()
    {b, c}
  end
end
