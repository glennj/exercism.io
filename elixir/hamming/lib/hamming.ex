defmodule Hamming do
  @type distance :: {:ok, non_neg_integer} | {:error, String.t()}

  @doc """
  Returns number of differences between two strands of DNA, known as the Hamming Distance.

  ## Examples

  iex> Hamming.hamming_distance('AAGTCATA', 'TAGCGATC')
  {:ok, 4}
  """
  @spec hamming_distance([char], [char]) :: distance
  def hamming_distance(strand1, strand2) do
    do_hamming(strand1, strand2, 0)
  end

  @spec do_hamming([char], [char], non_neg_integer) :: distance

  defp do_hamming([], [], dist), do: {:ok, dist}

  defp do_hamming([], _, _), do: {:error, "strands must be of equal length"}
  defp do_hamming(_, [], _), do: {:error, "strands must be of equal length"}

  defp do_hamming([a | as], [b | bs], count) when a != b do
    do_hamming(as, bs, count + 1)
  end

  defp do_hamming([_ | as], [_ | bs], count) do
    do_hamming(as, bs, count)
  end
end
