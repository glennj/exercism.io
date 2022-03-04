defmodule VariableLengthQuantity do
  import Bitwise

  @bits 7
  @mask 0b01111111
  @msb  0b10000000

  @doc """
  Encode integers into a bitstring of VLQ encoded bytes
  """
  @spec encode(integers :: [integer]) :: binary
  def encode(integers) do
    for i <- integers, reduce: <<>> do
      result -> <<result::bitstring, pack_bytes(i, <<>>)::bitstring>>
    end
  end

  defp pack_bytes(n, <<>>), do: pack_bytes(n >>> @bits, <<n &&& @mask::8>>)

  defp pack_bytes(0, bits), do: bits

  defp pack_bytes(n, bits) do
    pack_bytes(
      n >>> @bits,
      <<(n &&& @mask) ||| @msb::8, bits::bitstring>>
    )
  end

  @doc """
  Decode a bitstring of VLQ encoded bytes into a series of integers
  """
  @spec decode(bytes :: binary) :: {:ok, [integer]} | {:error, String.t()}
  def decode(bytes), do: do_decode(bytes, 0, [])

  defp do_decode(<<>>, _, integers), do: {:ok, Enum.reverse(integers)}

  defp do_decode(<<last::8>>, _, _) when (last &&& @msb) != 0 do
    {:error, "incomplete sequence"}
  end

  defp do_decode(<<byte::8, bits::bitstring>>, n, integers) do
    n = n <<< @bits ||| (byte &&& @mask)

    case byte &&& @msb do
      0 -> do_decode(bits, 0, [n | integers])
      _ -> do_decode(bits, n, integers)
    end
  end
end
