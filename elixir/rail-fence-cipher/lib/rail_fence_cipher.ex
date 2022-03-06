defmodule RailFenceCipher do
  # ------------------------------------------------------------
  @doc """
  Encode a given plaintext to the corresponding rail fence ciphertext
  """
  @spec encode(String.t(), pos_integer) :: String.t()
  def encode("", _), do: ""
  def encode(str, 1), do: str

  def encode(str, rails) do
    chars = String.graphemes(str)
    cycle = rail_cycle(rails)

    rail_map =
      for {char, rail} <- Enum.zip(chars, cycle),
          reduce: %{} do
        acc -> Map.update(acc, rail, char, &(&1 <> char))
      end

    Enum.reduce(0..(map_size(rail_map) - 1), "", &(&2 <> rail_map[&1]))
  end

  # example: 
  #   rails == 4
  #   cycle => 0, 1, 2, 3, 2, 1, 0, 1, 2, 3, 2, 1, ...
  defp rail_cycle(rails) do
    Enum.concat(0..(rails - 2), (rails - 1)..1) |> Stream.cycle()
  end

  # ------------------------------------------------------------
  @doc """
  Decode a given rail fence ciphertext to the corresponding plaintext
  """
  @spec decode(String.t(), pos_integer) :: String.t()
  def decode("", _), do: ""
  def decode(str, 1), do: str

  def decode(str, rails) do
    # one cycle down and up the rails consumes this many chars
    cycle_length = 2 * (rails - 1)
    cycles = div(String.length(str), cycle_length)
    remaining = rem(String.length(str), cycle_length)

    # break the string up into "segments", the sequences of characters
    # that reside on each rail.
    segment_lengths =
      0..(rails - 1)
      # "inner" rails consume twice as many characters
      |> Enum.map(fn i ->
        {i, if(i in [0, rails - 1], do: cycles, else: 2 * cycles)}
      end)
      # account for left-over characters
      |> Enum.map(fn {i, len} ->
        if i < remaining, do: len + 1, else: len
      end)

    # given the segment lengths, break up the string using a regex
    {:ok, segment_re} =
      segment_lengths
      |> Enum.map(&"(.{#{&1}})")
      |> Enum.join()
      |> Regex.compile()

    segments =
      Regex.run(segment_re, str)
      |> Enum.drop(1)
      |> Enum.map(&String.graphemes/1)

    # cycle through the rails, extracting the first char of the segment
    # at each iteration, to decode the string.
    {_, decoded} =
      Enum.reduce(
        rail_cycle(rails) |> Enum.take(String.length(str)),
        {segments, ""},
        fn i, {segments, decoded} ->
          [char | rest] = Enum.at(segments, i)
          {List.replace_at(segments, i, rest), decoded <> char}
        end
      )

    decoded
  end
end
