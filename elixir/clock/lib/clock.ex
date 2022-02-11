defmodule Clock do
  # yes, just minutes
  defstruct minutes: 0

  @doc """
  Returns a clock that can be represented as a string:
  """
  @spec new(integer, integer) :: Clock
  def new(hour, minute) do
    normalize(60 * hour + minute)
  end

  defp normalize(minutes) do
    %Clock{minutes: Integer.mod(minutes, 24 * 60)}
  end

  @doc """
  Adds two clock times:
  """
  @spec add(Clock, integer) :: Clock
  def add(%Clock{minutes: minutes}, add_minute) do
    normalize(minutes + add_minute)
  end
end

defimpl String.Chars, for: Clock do
  def to_string(%Clock{minutes: minutes}) do
    pad = fn n ->
      Integer.to_string(n)
      |> String.pad_leading(2, "0")
    end

    pad.(div(minutes, 60)) <> ":" <> pad.(rem(minutes, 60))
  end
end
