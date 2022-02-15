defmodule Queens do
  @type t :: %Queens{black: {integer, integer}, white: {integer, integer}}
  defstruct [:white, :black]

  @doc """
  Creates a new set of Queens
  """
  @spec new(Keyword.t()) :: Queens.t()
  def new(opts \\ []) do
    {black, white} = validate_opts(opts)

    %Queens{black: black, white: white}
  end

  defp validate_opts(opts) do
    if Enum.any?(opts, fn {k, _} -> k not in [:black, :white] end) do
      raise ArgumentError, "invalid colour"
    end

    black = validate_position(Keyword.get(opts, :black))
    white = validate_position(Keyword.get(opts, :white))

    if black == white and not is_nil(black) do
      raise ArgumentError, "same position"
    end

    {black, white}
  end

  defp validate_position(nil), do: nil

  defp validate_position({row, col}) do
    if not (row in 0..7 and col in 0..7) do
      raise ArgumentError, "not on the board"
    end

    {row, col}
  end

  @doc """
  Gives a string representation of the board with
  white and black queen locations shown
  """
  @spec to_string(Queens.t()) :: String.t()
  def to_string(queens) do
    List.duplicate("_", 8)            # one row
    |> List.duplicate(8)              # 8 rows
    |> place_queen(queens.black, "B") # place black queen
    |> place_queen(queens.white, "W") # place white queen
    |> Enum.map(&Enum.join(&1, " "))  # join each row
    |> Enum.join("\n")                # join all rows
  end

  defp place_queen(board, nil, _), do: board

  defp place_queen(board, {x, y}, queen) do
    row = Enum.at(board, x)
    row = List.replace_at(row, y, queen)
    List.replace_at(board, x, row)
  end

  @doc """
  Checks if the queens can attack each other
  """
  @spec can_attack?(Queens.t()) :: boolean
  def can_attack?(qs) when is_nil(qs.black) or is_nil(qs.white), do: false

  def can_attack?(queens) do
    d_row = abs(elem(queens.black, 0) - elem(queens.white, 0))
    d_col = abs(elem(queens.black, 1) - elem(queens.white, 1))

    d_row == 0 or d_col == 0 or d_row == d_col
  end
end
