defmodule OcrNumbers do
  @grid_lines 4
  @grid_cols 3

  @errors %{
    :lines => {:error, "invalid line count"},
    :cols => {:error, "invalid column count"}
  }

  # the 3 x 4 string representation of the digits, flattened
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
  def convert(lines) do
    with :ok <- validate_lines(length(lines)),
         :ok <- validate_columns(Enum.map(lines, &String.length/1)),
         digits <- convert_lines(lines),
    do: {:ok, digits}
  end

  defp validate_lines(len) when rem(len, @grid_lines) != 0, do: @errors[:lines]
  defp validate_lines(_), do: :ok

  defp validate_columns([first | _]) when rem(first, @grid_cols) != 0, do: @errors[:cols]
  defp validate_columns([first | rest]) do
    if Enum.any?(rest, &(&1 != first)),
      do: @errors[:cols],
      else: :ok
  end

  # Break the lines into chunks of 4, then convert each group to its digit string.
  defp convert_lines(input) do
    Enum.chunk_every(input, @grid_lines)
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
    |> Enum.map(&Enum.chunk_every(&1, @grid_cols))
    |> Enum.zip_with(&(Enum.concat(&1) |> Enum.join()))
  end

  defp to_digits(ocr_strings) do
    ocr_strings
    |> Enum.map(&Map.get(@numbers, &1, "?"))
  end
end
