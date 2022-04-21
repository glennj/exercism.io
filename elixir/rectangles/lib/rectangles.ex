defmodule Rectangles do
  @doc """
  Count the number of ASCII rectangles.
  """
  @spec count(input :: String.t()) :: integer
  def count(input) do
    board = split_input(input)
    vertices = find_vertices(board)
    count_rectangles(board, vertices)
  end

  defp split_input(input) do
    input
    |> String.split("\n")
    |> Enum.map(&String.to_charlist/1)
  end

  defp find_vertices(board) do
    for {row, r} <- Enum.with_index(board),
        {char, c} <- Enum.with_index(row),
        char == ?+
    do {r, c} end
  end

  defp count_rectangles(board, vertices) do
    for tl <- vertices,                   # top-left
        tr <- right_of(vertices, tl),     # top-right
        bl <- under(vertices, tl),        # bottom-left
        br = {elem(bl, 0), elem(tr, 1)},  # bottom-right
        br in vertices,
        rectangle?(board, tl, tr, bl, br),
        reduce: 0
    do count -> count + 1 end
  end

  defp right_of(vertices, {r, c}),
    do: Enum.filter(vertices, fn {rr, cc} -> rr == r && cc > c end)

  defp under(vertices, {r, c}),
    do: Enum.filter(vertices, fn {rr, cc} -> cc == c && rr > r end)

  defp rectangle?(board, tl, tr, bl, br) do
    horizontal_line?(tl, tr, board) &&
    horizontal_line?(bl, br, board) &&
    vertical_line?(tl, bl, board) &&
    vertical_line?(tr, br, board)
  end

  defp horizontal_line?({r, c1}, {r, c2}, board) do
    Enum.at(board, r)
    |> Enum.slice(c1, c2 - c1 + 1)
    |> Enum.all?(fn char -> char in '-+' end)
  end

  defp vertical_line?({r1, c}, {r2, c}, board) do
    Enum.slice(board, r1, r2 - r1 + 1)
    |> Enum.all?(fn row -> Enum.at(row, c) in '|+' end)
  end
end
