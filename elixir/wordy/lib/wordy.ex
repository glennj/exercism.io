defmodule Wordy do
  @type op :: (integer, integer -> integer) | nil

  @doc """
  Calculate the math problem in the sentence.
  """
  @spec answer(String.t()) :: integer
  def answer(question) do
    words =
      question
      |> String.downcase()
      |> String.replace(~r/^\s*what is/, "")
      |> String.replace(~r/[?]\s*$/, "")
      |> String.split()

    # start with a number
    get_number(words)
  end

  # ------------------------------------------------------------
  # The next word _should_ be a number.
  # - &String.to_integer/1 raises ArgumentError

  defp get_number(words, operation \\ nil, accumulator \\ 0)

  # Can't end an expression seeking the next number
  defp get_number([], _, _),
    do: raise(ArgumentError)

  # Initial iteration passes nil as the operation
  # Set the accumulator to the value of the first number
  defp get_number([num | words], nil, _),
    do: get_operation(words, String.to_integer(num))

  defp get_number([num | words], operation, acc),
    do: get_operation(words, operation.(acc, String.to_integer(num)))

  # ------------------------------------------------------------
  # The next word(s) _should_ be an arithmetic operation.

  # When there are no more words, we have the answer
  defp get_operation([], answer),
    do: answer

  defp get_operation(["plus" | words], acc),
    do: get_number(words, &Kernel.+/2, acc)

  defp get_operation(["minus" | words], acc),
    do: get_number(words, &Kernel.-/2, acc)

  defp get_operation(["multiplied", "by" | words], acc),
    do: get_number(words, &Kernel.*/2, acc)

  defp get_operation(["divided", "by" | words], acc),
    do: get_number(words, &Kernel.div/2, acc)

  defp get_operation(_, _),
    do: raise(ArgumentError)
end
