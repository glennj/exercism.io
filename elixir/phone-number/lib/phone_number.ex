defmodule PhoneNumber do
  @doc """
  Remove formatting from a phone number if the given number is valid. Return an error otherwise.
  """
  @spec clean(String.t()) :: {:ok | :error, String.t()}

  # Practicing happy path with `with`.

  def clean(raw) do
    with {:ok, digits} <- valid_characters(raw),
         {:ok, digits} <- valid_length(digits),
         {:ok, digits} <- remove_valid_country_code(digits),
         {:ok, digits} <- valid_codes(digits),
         do: {:ok, to_string(digits)}
  end

  defp valid_characters(phone) do
    # remove valid punctuation
    digits =
      String.replace(phone, ~r/[\s().+-]/, "")
      |> to_charlist()

    if Enum.all?(digits, &(&1 in ?0..?9)),
      do: {:ok, digits},
      else: {:error, "must contain digits only"}
  end

  defp valid_length(digits) when length(digits) in [10, 11], do: {:ok, digits}
  defp valid_length(_), do: {:error, "incorrect number of digits"}

  defp remove_valid_country_code(digits) when length(digits) == 10, do: {:ok, digits}
  defp remove_valid_country_code([?1 | digits]), do: {:ok, digits}
  defp remove_valid_country_code(_), do: {:error, "11 digits must start with 1"}

  defp valid_codes([?0 | _]), do: {:error, "area code cannot start with zero"}
  defp valid_codes([?1 | _]), do: {:error, "area code cannot start with one"}
  defp valid_codes([_, _, _, ?0 | _]), do: {:error, "exchange code cannot start with zero"}
  defp valid_codes([_, _, _, ?1 | _]), do: {:error, "exchange code cannot start with one"}
  defp valid_codes(digits), do: {:ok, digits}
end
