defmodule Atbash do
  @map (Enum.zip(?a..?z, ?z..?a) ++ Enum.map(?0..?9, &{&1, &1}))
       |> Enum.into(%{})

  @encodeable Map.keys(@map)

  @doc """
  Encode a given plaintext to the corresponding ciphertext
  """
  @spec encode(String.t()) :: String.t()
  def encode(plaintext) do
    # decoding and encoding are identical,
    # except that encoding adds spaces
    encoded = decode(plaintext)
    Regex.scan(~r/.{1,5}/, encoded) |> Enum.join(" ")
  end

  @spec decode(String.t()) :: String.t()
  def decode(cipher) do
    do_decode(cipher |> String.downcase() |> to_charlist(), '')
  end

  @spec do_decode(charlist, charlist) :: String.t()
  defp do_decode('', decoded) do
    decoded |> to_string() |> String.reverse()
  end

  defp do_decode([c | chars], decoded) when c in @encodeable do
    do_decode(chars, [@map[c] | decoded])
  end

  defp do_decode([_ | chars], decoded) do
    do_decode(chars, decoded)
  end
end
