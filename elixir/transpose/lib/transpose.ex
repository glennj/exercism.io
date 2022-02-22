defmodule Matrix do
  @type t() :: list(list(any()))

  @doc """
  Transpose a rectangular matrix.

  Example:

      > Matrix.transpose([[:a, :b, :c], [:d, :e, :f]])
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

defmodule Transpose do
  @spec transpose(String.t()) :: String.t()
  def transpose(input) do
    lines = String.split(input, "\n")

    max_len =
      lines
      |> Enum.map(&String.length/1)
      |> Enum.max()

    lines
    |> Enum.map(&String.pad_trailing(&1, max_len))
    |> Enum.map(&String.graphemes/1)
    |> Matrix.transpose()
    |> Enum.map(&Enum.join/1)
    |> special_padright()
    |> Enum.join("\n")
  end

  # each line has to be padded so that it is 
  # _not shorter_ than the line _following it_.
  defp special_padright([]), do: []
  defp special_padright([line]), do: [String.trim_trailing(line)]

  defp special_padright([line | lines]) do
    padded = special_padright(lines)
    len = String.length(hd(padded))
    line =
      line
      |> String.trim_trailing()
      |> String.pad_trailing(len)

    [line | padded]
  end
end
