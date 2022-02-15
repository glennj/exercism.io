defmodule DiffieHellman do
  @doc """
  Given a prime integer `prime_p`, return a random integer between 1 and `prime_p` - 1
  """
  @spec generate_private_key(prime_p :: integer) :: integer
  def generate_private_key(p) do
    Enum.random(1..(p - 1))
  end

  @doc """
  Given two prime integers as generators (`prime_p` and `prime_g`), and a private key,
  generate a public key using the mathematical formula:

  (prime_g **  private_key) % prime_p
  """
  @spec generate_public_key(prime_p :: integer, prime_g :: integer, private_key :: integer) :: integer
  def generate_public_key(p, g, k) do
    ## take 1: naive attempt to perform math directly
    # rem(g ** k, p)

    ## iteration 2: optimize math
    ## ref https://www.math.ucla.edu/~baker/40/handouts/rev_DH/node1.html
    ## still too slow
    # calculate(g, k, p)

    ## iteration 3: read community solutions and discover
    ## Erlang module made just for this purpose
    :crypto.mod_pow(g, k, p)
    |> binary_to_integer()
  end

  # experimenting with the implementation of decoding bitstrings.
  # normal people would do:  a_binary |> :binary.decode_unsigned()

  defp binary_to_integer(binary), do: do_bin_to_int(binary, 0)

  defp do_bin_to_int(<<>>, result), do: result

  defp do_bin_to_int(<<byte, rest::bitstring>>, result) do
    do_bin_to_int(rest, Bitwise.bsl(result, 8) |> Bitwise.bor(byte))
  end

  @doc """
  Given a prime integer `prime_p`, user B's public key, and user A's private key,
  generate a shared secret using the mathematical formula:

  (public_key_b ** private_key_a) % prime_p
  """
  @spec generate_shared_secret(prime_p :: integer, public_key_b :: integer, private_key_a :: integer) :: integer
  def generate_shared_secret(p, k_b, k_a) do
    generate_public_key(p, k_b, k_a)
  end

  # optimization to calculate:  (g ** k) mod p
  # but not good enough

  defp calculate(g, 1, p), do: rem(g, p)

  defp calculate(g, k, p) do
    do_calculate(g, k, p, rem(g ** 2, p), 2)
  end

  defp do_calculate(_g, k, _p, result, k), do: result

  defp do_calculate(g, k, p, result, i) when 2 * i > k do
    do_calculate(g, k, p, rem(g * result, p), i + 1)
  end

  defp do_calculate(g, k, p, result, i) do
    do_calculate(g, k, p, rem(result ** 2, p), 2 * i)
  end
end
