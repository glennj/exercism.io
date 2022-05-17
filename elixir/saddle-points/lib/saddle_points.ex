defmodule SaddlePoints do
  import Matrix, only: [transpose: 1]

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
  def columns(str), do: str |> rows() |> transpose()

  @doc """
  Calculates all the saddle points from a string
  representation of a matrix
  """
  @spec saddle_points(String.t()) :: [{integer, integer}]
  def saddle_points(""), do: []
  def saddle_points(str) do
    rows = rows(str)
    cols = transpose(rows)

    row_maxima = Enum.map(rows, &Enum.max/1)
    col_minima = Enum.map(cols, &Enum.min/1)

    for {row, r} <- Enum.with_index(rows),
        {val, c} <- Enum.with_index(row),
        Enum.at(row_maxima, r) == val,
        Enum.at(col_minima, c) == val
    do
      {r + 1, c + 1}
    end
  end
end
