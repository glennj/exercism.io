defmodule Proverb do
  @doc """
  Generate a proverb from a list of strings.
  """
  @spec recite(strings :: [String.t()]) :: String.t()
  def recite([]), do: ""

  def recite(strings) do
    [first | _] = strings

    for [this, that] <- Enum.chunk_every(strings, 2, 1, :discard),
        reduce: ""
    do
      acc -> acc <> "For want of a #{this} the #{that} was lost.\n"
    end
    <> "And all for the want of a #{first}.\n"
  end
end
