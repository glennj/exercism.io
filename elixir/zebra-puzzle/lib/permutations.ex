# from https://www.erlang.org/doc/programming_examples/list_comprehensions.html#permutations
defmodule Permutations do
  def permutations([]), do: [[]]

  def permutations(list) do
    for h <- list,
        t <- permutations(list -- [h]) do
      [h | t]
    end
  end
end
