defmodule Matrix do
  @type t() :: list(list(any()))

  @doc """
  Transpose a rectangular matrix.

  ## Example

      iex> Matrix.transpose([])
      []
      iex> Matrix.transpose([[:a, :b, :c]])
      [[:a], [:b], [:c]]
      iex> Matrix.transpose([[:a, :b, :c], [:d, :e, :f]])
      [[:a, :d], [:b, :e], [:c, :f]]

  """
  @spec transpose(matrix :: t()) :: t()

  # def transpose(matrix), do: Enum.zip(matrix) |> Enum.map(&Tuple.to_list/1)

  # Simpler:
  def transpose(matrix), do: Enum.zip_with(matrix, & &1)

  ## "Manual" implementation
  # def transpose([]), do: []
  # def transpose(matrix), do: do_transpose(matrix, 0, length(hd(matrix)), [])
  #
  # defp do_transpose(_, idx, len, columns) when idx == len,
  #   do: Enum.reverse(columns)
  #
  # defp do_transpose(m, idx, len, columns) do
  #   col = for row <- m do Enum.at(row, idx) end
  #   do_transpose(m, idx + 1, len, [col | columns])
  # end
end
