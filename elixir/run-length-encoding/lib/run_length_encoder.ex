defmodule RunLengthEncoder do
  @doc """
  Generates a string where consecutive elements are represented as a data value and count.
  "AABBBCCCC" => "2A3B4C"
  """
  @spec encode(String.t()) :: String.t()
  def encode(string) do
    Regex.replace(
      ~r/(.)\1+/,   # matches runs of _two_ or more of a character
      string,
      fn run, c -> "#{String.length(run)}#{c}" end
    )
  end

  @spec decode(String.t()) :: String.t()
  def decode(string) do
    Regex.replace(
      ~r/(\d+)(\D)/,  # matches digits followed by a non-digit
      string,
      fn _, len, c -> String.duplicate(c, String.to_integer(len)) end
    )
  end
end
