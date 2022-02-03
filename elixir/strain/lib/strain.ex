defmodule Strain do
  @doc """
  Given a `list` of items and a function `fun`, return the list of items where
  `fun` returns true.

  Do not use `Enum.filter`.
  """
  @spec keep(list :: list(any), fun :: (any -> boolean)) :: list(any)
  def keep(list, fun) do
    do_keep(list, fun, [])
  end

  defp do_keep([], _, result), do: result
  defp do_keep([h | t], fun, acc) do
    acc = if fun.(h), do: acc ++ [h], else: acc
    do_keep(t, fun, acc)
  end
            
  @doc """
  Given a `list` of items and a function `fun`, return the list of items where
  `fun` returns false.

  Do not use `Enum.reject`.
  """
  @spec discard(list :: list(any), fun :: (any -> boolean)) :: list(any)
  def discard(list, fun) do
    do_discard(list, fun, [])
  end

  defp do_discard([], _, result), do: result
  defp do_discard([h | t], fun, acc) do
    acc = if fun.(h), do: acc, else: acc ++ [h]
    do_discard(t, fun, acc)
  end
end
