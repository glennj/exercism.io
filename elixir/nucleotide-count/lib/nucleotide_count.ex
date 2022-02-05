defmodule NucleotideCount do
  @nucleotides [?A, ?C, ?G, ?T]

  @doc """
  Counts individual nucleotides in a DNA strand.
  """
  @spec count(charlist(), char()) :: non_neg_integer()
  def count(strand, nucleotide), do: histogram(strand)[nucleotide]

  @doc """
  Returns a summary of counts by nucleotide.
  """
  @spec histogram(charlist()) :: map()
  def histogram(strand), do: do_histogram(strand, %{})

  defp do_histogram([], hist) do
    # ensure all the nucleotides are in the map
    Enum.reduce(
      @nucleotides,
      hist,
      fn n, h -> Map.put_new(h, n, 0) end
    )
  end

  defp do_histogram([n | strand], hist) do
    do_histogram(strand, Map.update(hist, n, 1, &(&1 + 1)))
  end
end
