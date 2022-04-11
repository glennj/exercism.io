defmodule OcrNumbers do
  @numbers %{
    " _ | ||_|   " => "0",
    "     |  |   " => "1",
    " _  _||_    " => "2",
    " _  _| _|   " => "3",
    "   |_|  |   " => "4",
    " _ |_  _|   " => "5",
    " _ |_ |_|   " => "6",
    " _   |  |   " => "7",
    " _ |_||_|   " => "8",
    " _ |_| _|   " => "9"
  }

  @doc """
  Given a 3 x 4 grid of pipes, underscores, and spaces, determine which number is represented, or
  whether it is garbled.
  """
  @spec convert([String.t()]) :: {:ok, String.t()} | {:error, charlist()}
  def convert(input) do
    cond do
      rem(length(input), 4) != 0 ->
        {:error, "invalid line count"}

      Enum.any?(input, &(rem(String.length(&1), 3) != 0)) ->
        {:error, "invalid column count"}

      true ->
        {:ok, convert_lines(input)}
    end
  end

  # Break the lines into chunks of 4, then convert each group to its digit string.
  defp convert_lines(input) do
    Enum.chunk_every(input, 4)
    |> Enum.map(&convert_line/1)
    |> Enum.join(",")
  end

  # Convert a group of 4 lines to its digits string
  defp convert_line(lines) do
    lines
    |> to_ocr_strings()
    |> to_digits()
    |> to_string()
  end

  defp to_ocr_strings(lines) do
    lines
    |> Enum.map(&String.graphemes/1)
    |> Enum.map(&Enum.chunk_every(&1, 3))
    |> Enum.zip_with(&(Enum.concat(&1) |> Enum.join()))
  end

  defp to_digits(ocr_strings) do
    ocr_strings
    |> Enum.map(&Map.get(@numbers, &1, "?"))
  end
end
