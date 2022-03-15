defmodule PhoneNumber do
  @doc """
  Remove formatting from a phone number if the given number is valid. Return an error otherwise.
  """
  @spec clean(String.t()) :: {:ok, String.t()} | {:error, String.t()}

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

    if Enum.any?(digits, &(&1 not in ?0..?9)),
      do: {:error, "must contain digits only"},
      else: {:ok, digits}
  end

  defp valid_length(digits) when length(digits) not in [10, 11],
    do: {:error, "incorrect number of digits"}
  defp valid_length(digits),
    do: {:ok, digits}

  defp remove_valid_country_code(digits) when length(digits) == 10,
    do: {:ok, digits}
  defp remove_valid_country_code([?1 | digits]),
    do: {:ok, digits}
  defp remove_valid_country_code(_),
    do: {:error, "11 digits must start with 1"}

  defp valid_codes([?0 | _]), do: {:error, "area code cannot start with zero"}
  defp valid_codes([?1 | _]), do: {:error, "area code cannot start with one"}
  defp valid_codes([_, _, _, ?0 | _]), do: {:error, "exchange code cannot start with zero"}
  defp valid_codes([_, _, _, ?1 | _]), do: {:error, "exchange code cannot start with one"}
  defp valid_codes(digits), do: {:ok, digits}

  @moduledoc """
  Iteration one was shorter but a bit messier:

      def clean(raw) do
        cleaned = remove_valid_punctuation(raw)

        cond do
          nondigits(cleaned) -> {:error, "must contain digits only"}
          wrong_length(cleaned) -> {:error, "incorrect number of digits"}
          bad_country_code(cleaned) -> {:error, "11 digits must start with 1"}
          true -> check_codes(remove_country_code(cleaned))
        end
      end

      defp check_codes(phone) do
        case {String.at(phone, 0), String.at(phone, 3)} do
          {"0", _} -> {:error, "area code cannot start with zero"}
          {"1", _} -> {:error, "area code cannot start with one"}
          {_, "0"} -> {:error, "exchange code cannot start with zero"}
          {_, "1"} -> {:error, "exchange code cannot start with one"}
          _ -> {:ok, phone}
        end
      end

      defp remove_country_code(phone),
        do: String.replace(phone, ~r/^1(\d{10})$/, "\\1")

      defp nondigits(phone),
        do: String.match?(phone, ~r/\D/)

      defp wrong_length(phone),
        do: String.length(phone) not in 10..11

      defp bad_country_code(phone),
        do: String.length(phone) == 11 and not String.starts_with?(phone, "1")
  """
end
