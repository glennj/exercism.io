defmodule ListOps do
  # Please don't use any external modules (especially List or Enum) in your
  # implementation. The point of this exercise is to create these basic
  # functions yourself. You may use basic Kernel functions (like `Kernel.+/2`
  # for adding numbers), but please do not use Kernel functions for Lists like
  # `++`, `--`, `hd`, `tl`, `in`, and `length`.

  @spec reverse(list) :: list
  def reverse(l), do: do_reverse(l, [])

  defp do_reverse([], rev), do: rev
  defp do_reverse([h | t], rev), do: do_reverse(t, [h | rev])

  @type acc :: any
  @spec foldl(list, acc, (any, acc -> acc)) :: acc
  def foldl([], acc, _), do: acc
  def foldl([h | t], acc, f), do: foldl(t, f.(h, acc), f)

  ##################################################
  # All of the other operations can be implemented #
  # with `reverse` and `foldl`                     #
  ##################################################

  @spec count(list) :: non_neg_integer
  def count(l) do
    foldl(l, 0, fn _, acc -> acc + 1 end)
  end

  @spec map(list, (any -> any)) :: list
  def map(l, f) do
    foldl(l, [], fn elem, acc -> [f.(elem) | acc] end)
    |> reverse()
  end

  @spec filter(list, (any -> as_boolean(term))) :: list
  def filter(l, f) do
    foldl(l, [], fn elem, acc ->
      if f.(elem) do
        [elem | acc]
      else
        acc
      end
    end)
    |> reverse()
  end

  @spec append(list, list) :: list
  def append(a, b) do
    foldl(b, reverse(a), fn elem, acc -> [elem | acc] end)
    |> reverse()
  end

  @spec foldr(list, acc, (any, acc -> acc)) :: acc
  def foldr(l, acc, f) do
    foldl(reverse(l), acc, fn elem, acc -> f.(elem, acc) end)
  end

  @spec concat([[any]]) :: [any]
  def concat(ll) do
    # _could_ be implemented like this:
    #
    #   foldl(ll, [], fn (list, acc) -> append(acc, list) end)
    #
    # but it's very very slow for "huge list of small lists"
    # - too many reverse operations
    do_concat(ll, [])
  end

  defp do_concat([], result), do: reverse(result)

  defp do_concat([list | ll], result) do
    do_concat(ll, do_insert(list, result))
  end

  defp do_insert([], result), do: result
  defp do_insert([h | t], result), do: do_insert(t, [h | result])
end
