defmodule Matrix do
  def new(rows, cols) do
    List.duplicate(nil, cols)
    |> List.duplicate(rows)
  end

  def replace_at(matrix, {x, y}, value) do
    row = Enum.at(matrix, y)
    updated = List.replace_at(row, x, value)
    List.replace_at(matrix, y, updated)
  end
end

defmodule Spiral do
  @doc """
  Given the dimension, return a square matrix of numbers in clockwise spiral order.
  """
  @spec matrix(dimension :: integer) :: list(list(integer))
  def matrix(n) do
    walk(Matrix.new(n, n), 1, {-1, 0}, steps(n))
  end

  defp walk(mtx, _, _, []), do: mtx

  defp walk(mtx, i, {x, y}, [{dx, dy} | steps]) do
    cell = {x + dx, y + dy}

    walk(Matrix.replace_at(mtx, cell, i), i + 1, cell, steps)
  end

  defp steps(n) do
    # This stream will generate [4, 3, 3, 2, 2, 1, 1] for n == 4
    # This represents the number of steps the "cursor" makes
    # before turning in a different direction.
    # Note the sum of these values == n * n
    s =
      Stream.iterate([n, n - 1], fn [_, b] -> [b, b - 1] end)
      |> Stream.flat_map(& &1)
      |> Stream.take_while(&(&1 > 0))

    # This cycle represents the "dx" and "dy" values as the
    # "cursor" moves right/down/left/up the spiral.
    d = Stream.cycle([{1, 0}, {0, 1}, {-1, 0}, {0, -1}])

    for {n, {dx, dy}} <- Enum.zip(s, d) do
      List.duplicate({dx, dy}, n)
    end
    |> Enum.flat_map(& &1)
  end
end
