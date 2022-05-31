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
      for {char, rail} <- Enum.zip(chars, cycle), reduce: %{} do
        acc -> Map.update(acc, rail, char, &(&1 <> char))
      end

    # Given: str = "EXERCISMISAWESOME!" and rails = 5
    #
    # the result of the Enum.zip call will be:
    #   [{"E",0}, {"X",1}, {"E",2}, {"R",3}, {"C",4}, {"I",3}, {"S",2}, {"M",1},
    #    {"I",0}, {"S",1}, {"A",2}, {"W",3}, {"E",4}, {"S",3}, {"O",2}, {"M",1},
    #    {"E",0}, {"!",1}]
    #
    # and rail_map = %{0 => "EIE", 1 => "XMSM!", 2 => "ESAO", 3 => "RIWS", 4 => "CE"}

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
    # The running example uses: str = "EIEXMSM!ESAORIWSCE" and rails = 5

    # one cycle down and up the rails consumes this many chars
    str_len = String.length(str)            # => 18
    cycle_length = 2 * (rails - 1)          # =>  8
    cycles = div(str_len, cycle_length)     # =>  2
    remaining = rem(str_len, cycle_length)  # =>  2

    segment_lengths =
      # "inner" rails consume twice as many characters
      [cycles | List.duplicate(2 * cycles, rails - 2) ++ [cycles]]
      # account for left-over characters
      |> Enum.with_index()
      |> Enum.map(fn
          {n, i} when i < remaining -> n + 1
          {n, _} -> n
        end
      )
    #
    # segment_lengths = [3, 5, 4, 4, 2]

    # given the segment lengths, break up the string using a regex
    {:ok, segment_re} =
      segment_lengths
      |> Enum.map(&"(.{#{&1}})")
      |> Enum.join()
      |> Regex.compile()
    #
    # segment_re = ~r/(.{3})(.{5})(.{4})(.{4})(.{2})/

    segments =
      Regex.run(segment_re, str)
      |> Enum.drop(1)
      |> Enum.map(&String.graphemes/1)
    #
    # segments =
    #   [["E","I","E"], ["X","M","S","M","!"], ["E","S","A","O"], ["R","I","W","S"], ["C","E"]]
    #
    # or, represented in a grid to see how we'll extract chars using the rail_cycle
    #          <down  up  down  up  down>
    #         [["E",      "I",      "E"],
    #          ["X", "M", "S", "M", "!"],
    #          ["E", "S", "A", "O"     ],
    #          ["R", "I", "W", "S"     ],
    #          [     "C",      "E"     ]]

    # cycle through the rails, extracting the first char of the segment
    # at each iteration, to decode the string.
    {_, decoded} =
      Enum.reduce(
        rail_cycle(rails) |> Enum.take(str_len),
        {segments, ""},
        fn i, {segments, decoded} ->
          [char | rest] = Enum.at(segments, i)
          {List.replace_at(segments, i, rest), decoded <> char}
        end
      )

    decoded
  end
end
