defmodule Matrix do
  @type t() :: list(list(any()))

  @doc """
  Transpose a rectangular matrix.

  ## Example

      iex> Matrix.transpose([[:a, :b, :c], [:d, :e, :f]])
      [[:a, :d], [:b, :e], [:c, :f]]

  """
  @spec transpose(matrix :: t()) :: t()
  def transpose(matrix), do: do_transpose(matrix, [])
  
  defp do_transpose([], _), do: []

  defp do_transpose([[] | _], transposed), do: Enum.reverse(transposed)

  defp do_transpose(rows, cols) do
    col  = for row <- rows do hd(row) end
    rows = for row <- rows do tl(row) end

    do_transpose(rows, [col | cols])
  end
end
