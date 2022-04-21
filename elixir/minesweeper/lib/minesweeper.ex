defmodule Minesweeper do
  @doc """
  Annotate empty spots next to mines with the number of mines next to them.
  """
  @spec annotate([String.t()]) :: [String.t()]

  def annotate(board) do
    nums = for row <- board do
      for char <- to_charlist(row) do
        if char == ?*, then: 9, else: 0
      end
    end

  end
end
