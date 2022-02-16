## using an Agent to cache the palindromes
# defmodule Palindromes do
#  use Agent
#
#  def new() do
#    {:ok, pid} = Agent.start_link(&Map.new/0)
#    pid
#  end
#
#  def add_factors(pid, number, factors) do
#    Agent.update(pid, fn state ->
#      Map.update(state, number, [factors], &[factors | &1])
#    end)
#  end
#
#  def get_factors_map(pid) do
#    Agent.get(pid, & &1)
#  end
# end

defmodule PalindromeProducts do
  @doc """
  Generates all palindrome products from an optionally given min factor (or 1) to a given max factor.
  """
  @spec generate(non_neg_integer, non_neg_integer) :: map
  def generate(max_factor, min_factor \\ 1)

  def generate(max_f, min_f) when min_f > max_f do
    raise ArgumentError
  end

  ## using an Agent to cache the palindromes
  # def generate(max_f, min_f) do
  #   pals = Palindromes.new()
  #
  #   for i <- min_f..max_f,
  #       j <- i..max_f
  #   do
  #     if palindrome?(i * j) do
  #       Palindromes.add_factors(pals, i * j, [i, j])
  #     end
  #   end
  #
  #   Palindromes.get_factors_map(pals)
  # end

  # using for...reduce
  # - this is 1-2 seconds faster for the 4-digit tests
  def generate(max_f, min_f) do
    for i <- min_f..max_f,
        j <- i..max_f,
        palindrome?(i * j),
        reduce: %{}
    do
      pals -> Map.update(pals, i * j, [[i, j]], &[[i, j] | &1])
    end
  end

  defp palindrome?(number) do
    digits = Integer.digits(number)
    digits == Enum.reverse(digits)
  end
end
