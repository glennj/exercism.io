defmodule ETL do
  @doc """
  Transforms an old Scrabble score system to a new one.

  ## Examples

    iex> ETL.transform(%{1 => ["A", "E"], 2 => ["D", "G"]})
    %{"a" => 1, "d" => 2, "e" => 1, "g" => 2}
  """
  @spec transform(map) :: map
  def transform(input) do

    # my original, less efficient, implementation:
    # this takes an extra iteration over the input map:
    #
    #for score <- Map.keys(input),
    #    tile <- input[score],
    #    into: %{}
    # do ...

    for {score, tiles} <- input,
        tile <- tiles,
        into: %{}
    do
      {String.downcase(tile), score}
    end
  end
end
