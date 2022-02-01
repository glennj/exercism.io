defmodule BinarySearch do
  @doc """
    Searches for a key in the tuple using the binary search algorithm.
    It returns :not_found if the key is not in the tuple.
    Otherwise returns {:ok, index}.
  """

  @spec search(tuple, integer) :: {:ok, integer} | :not_found
  def search(numbers, key) do
    search(numbers, key, 0, tuple_size(numbers) - 1)
  end

  # private search/4:
  defp search(_, _, i, j) when i > j, do: :not_found

  defp search(numbers, key, i, j) do
    mid = div(i + j, 2)

    cond do
      elem(numbers, mid) > key -> search(numbers, key, i, mid - 1)
      elem(numbers, mid) < key -> search(numbers, key, mid + 1, j)
      true -> {:ok, mid}
    end
  end
end
