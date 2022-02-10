defmodule Matrix do
  defstruct [rows: [], columns: []]

  @doc """
  Convert an `input` string, with rows separated by newlines and values
  separated by single spaces, into a `Matrix` struct.
  """
  @spec from_string(input :: String.t()) :: %Matrix{}
  def from_string(""), do: %Matrix{}

  def from_string(input) do
    rows =
      input
      |> String.split("\n")
      |> Enum.map(fn row ->
        row
        |> String.split()
        |> Enum.map(&String.to_integer/1)
      end)

    %Matrix{rows: rows, columns: transpose(rows)}
  end

  defp transpose(rows) do
    do_transpose(rows, [])
  end

  defp do_transpose([], _), do: []

  defp do_transpose(rows, columns) when hd(rows) == [] do
    Enum.reverse(columns)
  end

  defp do_transpose(rows, columns) do
    column = for row <- rows do hd(row) end
    rows = for row <- rows do tl(row) end
    do_transpose(rows, [column | columns])
  end

  @doc """
  Write the `matrix` out as a string, with rows separated by newlines and
  values separated by single spaces.
  """
  @spec to_string(matrix :: %Matrix{}) :: String.t()
  def to_string(matrix) do
    matrix.rows
    |> Enum.map(fn row -> Enum.join(row, " ") end)
    |> Enum.join("\n")
  end

  @doc """
  Given a `matrix`, return its rows as a list of lists of integers.
  """
  @spec rows(matrix :: %Matrix{}) :: list(list(integer))
  def rows(matrix) do
    matrix.rows
  end

  @doc """
  Given a `matrix` and `index`, return the row at `index`.
  """
  @spec row(matrix :: %Matrix{}, index :: integer) :: list(integer)
  def row(matrix, index) do
    # at(matrix.rows, index)
    Enum.at(matrix.rows, index - 1)
  end

  ## Hmm, I reimplemented &Enum.at/3
  # defp at(elems, index, current \\ 0)
  # defp at([], _, _), do: nil
  # defp at([elem | _], index, i) when i + 1 == index, do: elem
  # defp at([_ | elems], index, i), do: at(elems, index, i + 1)

  @doc """
  Given a `matrix`, return its columns as a list of lists of integers.
  """
  @spec columns(matrix :: %Matrix{}) :: list(list(integer))
  def columns(matrix) do
    matrix.columns
  end

  @doc """
  Given a `matrix` and `index`, return the column at `index`.
  """
  @spec column(matrix :: %Matrix{}, index :: integer) :: list(integer)
  def column(matrix, index) do
    # at(matrix.columns, index)
    Enum.at(matrix.columns, index - 1)
  end
end
