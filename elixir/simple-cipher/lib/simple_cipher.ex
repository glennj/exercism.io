defmodule SimpleCipher do
  def encode(plaintext, key), do: encipher(plaintext, key, +1)
  def decode(ciphertext, key), do: encipher(ciphertext, key, -1)

  #------------------------------------------------------------
  @spec encipher(text :: String.t(), key :: String.t(), direction :: +1 | -1) :: String.t()
  defp encipher(text, key, direction) do
    Enum.zip(
      text_stream(text),
      key_stream(key, direction)
    )
    |> Enum.map(fn {c, d} -> Integer.mod(c + d, 26) end)
    |> Enum.map(&(&1 + ?a))
    |> to_string()
  end

  defp text_stream(text) do
    text
    |> String.downcase()
    |> to_charlist()
    |> Stream.filter(&(&1 in ?a..?z))
    |> Stream.map(&(&1 - ?a))
  end

  defp key_stream(key, direction) do
    key
    |> to_charlist()
    |> Stream.map(&((&1 - ?a) * direction))
    |> Stream.cycle()
  end
  #------------------------------------------------------------

  def generate_key(length) do
    for _ <- 1..length do Enum.random(?a..?z) end |> to_string()
  end
end
