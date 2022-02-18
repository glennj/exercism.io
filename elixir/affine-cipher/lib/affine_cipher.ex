defmodule AffineCipher do
  @typedoc """
  A type for the encryption key
  """
  @type key() :: %{a: integer, b: integer}

  # alphabet size
  @m 26

  @doc """
  Decode an encrypted message using a key
  """
  @spec decode(key :: key(), message :: String.t()) :: {:ok, String.t()} | {:error, String.t()}
  def decode(%{a: a, b: b}, encrypted) do
    a_inv = mmi(a, @m, 1)
    dec = fn y -> Integer.mod(a_inv * (y - b), @m) end
    encipher(encrypted, a, dec)
  end

  @doc """
  Encode an encrypted message using a key
  """
  @spec encode(key :: key(), message :: String.t()) :: {:ok, String.t()} | {:error, String.t()}
  def encode(%{a: a, b: b}, message) do
    enc = fn x -> Integer.mod(a * x + b, @m) end
    encipher(message, a, enc, &grouped/1)
  end

  #------------------------------------------------------------
  @spec encipher(
    text :: String.t(),
    a :: integer,
    func :: (integer -> integer),
    post_process :: (String.t() -> String.t())
  ) :: String.t()
  defp encipher(text, a, func, post_process \\ &(&1)) do
    if Integer.gcd(a, @m) != 1 do
      {:error, "a and m must be coprime."}
    else
      enciphered =
        for c <- String.downcase(text) |> to_charlist(),
            c in ?a..?z or c in ?0..?9
        do
          cond do
            c in ?a..?z -> ?a + func.(c - ?a)
            true -> c
          end
        end
        |> to_string()
        |> post_process.()

      {:ok, enciphered}
    end
  end

  defp grouped(string, size \\ 5) do
    Regex.scan(~r/.{1,#{size}}/, string)
    |> Enum.join(" ")
  end

  # we'll hit this base case if a and m not coprime
  defp mmi(_, m, x) when x == m, do: 0

  defp mmi(a, m, x) when rem(a * x, m) == 1, do: x
  defp mmi(a, m, x), do: mmi(a, m, x + 1)
end
