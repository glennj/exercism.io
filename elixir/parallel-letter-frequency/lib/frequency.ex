defmodule Frequency do
  @doc """
  Count letter frequency in parallel.

  Returns a map of characters to frequencies.

  The number of worker processes to use can be set with 'workers'.
  """
  @spec frequency([String.t()], pos_integer) :: map
  def frequency(texts, workers) do
    # start a task for each text given.
    # what to do about max workers? answer: Text.async_stream
    opts = [max_concurrency: workers, ordered: false]
    stream = Task.async_stream(texts, &count_letters/1, opts)

    # collect counts from each task.
    # collecting the stream awaits each task, returning its value.
    Enum.reduce(stream, %{}, fn {:ok, value}, freq ->
      Map.merge(freq, value, fn _, v1, v2 -> v1 + v2 end)
    end)
  end

  defp count_letters(text) do
    text
    |> String.graphemes()
    |> Stream.filter(fn c -> Regex.match?(~r/[[:alpha:]]/, c) end)
    |> Stream.map(&String.downcase/1)
    |> Enum.reduce(%{}, fn c, acc ->
      Map.update(acc, c, 1, &(&1 + 1))
    end)
  end
end
