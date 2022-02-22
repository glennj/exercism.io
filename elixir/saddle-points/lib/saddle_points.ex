defmodule SaddlePoints do
  @doc """
  Parses a string representation of a matrix
  to a list of rows
  """
  @spec rows(String.t()) :: [[integer]]
  def rows(""), do: []
  def rows(str) do
    str
    |> String.split("\n")
    |> Enum.map(fn line ->
      line
      |> String.split()
      |> Enum.map(&String.to_integer/1)
    end)
  end

  @doc """
  Parses a string representation of a matrix
  to a list of columns
  """
  @spec columns(String.t()) :: [[integer]]
  def columns(str) do
    str
    |> rows()
    |> Matrix.transpose()
  end

  @doc """
  Calculates all the saddle points from a string
  representation of a matrix
  """
  @spec saddle_points(String.t()) :: [{integer, integer}]
  def saddle_points(""), do: []
  def saddle_points(str) do
    rows = rows(str)
    cols = Matrix.transpose(rows)

    row_maxima = Enum.map(rows, &Enum.max/1)
    col_minima = Enum.map(cols, &Enum.min/1)

    for r <- 0..(length(rows) - 1),
        c <- 0..(length(cols) - 1),
        v = Enum.at(rows, r) |> Enum.at(c),
        v == Enum.at(row_maxima, r),
        v == Enum.at(col_minima, c)
    do
      {r + 1, c + 1}
    end
  end
end

#------------------------------------------------------------
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
