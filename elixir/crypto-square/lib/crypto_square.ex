defmodule CryptoSquare do
  import Matrix, only: [transpose: 1]

  @doc """
  Encode string square methods
  """
  @spec encode(String.t()) :: String.t()
  def encode(""), do: ""

  def encode(str) do
    cleaned =
      String.downcase(str)
      |> String.replace(~r/[^[:alnum:]]/, "")

    segment_length =
      String.length(cleaned)
      |> :math.sqrt()
      |> :math.ceil()
      |> trunc()

    # Regex.scan returns a list of lists.
    # When there are no capturing paretheses, the inner lists only contain one element:
    # `concat` appropriately flattens the list of lists.

    Regex.scan(~r/.{1,#{segment_length}}/, cleaned)
    |> Enum.concat()
    |> Enum.map(&String.pad_trailing(&1, segment_length))
    |> Enum.map(&String.graphemes/1)
    |> transpose()
    |> Enum.map(&Enum.join/1)
    |> Enum.join(" ")
  end
end
