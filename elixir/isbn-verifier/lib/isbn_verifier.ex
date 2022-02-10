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
      0 ===
        Enum.map(to_charlist(isbn), fn char ->
          case char do
            ?X -> 10
            c -> c - ?0
          end
        end)
        |> Enum.with_index()
        |> Enum.reduce(0, fn {digit, idx}, sum ->
          sum + digit * (10 - idx)
        end) 
        |> Integer.mod(11)
    end
  end
end
