defmodule IsbnVerifier do
  @doc """
  Checks if a string is a valid ISBN-10 identifier
  """
  @spec isbn?(String.t()) :: boolean
  def isbn?(isbn) do
    isbn = String.replace(isbn, "-", "")

    if not String.match?(isbn, ~r/^\d{9}[\dX]$/) do
      false
    else
      for {char, i} <- to_charlist(isbn) |> Enum.with_index() do
        isbn_int(char) * (10 - i)
      end
      |> Enum.sum() 
      |> Integer.mod(11) === 0
    end
  end

  defp isbn_int(c) when c in ?0..?9, do: c - ?0
  defp isbn_int(?X), do: 10
end
