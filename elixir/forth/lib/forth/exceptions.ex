defmodule Forth.StackUnderflow do
  defexception []
  def message(_), do: "stack underflow"
end

defmodule Forth.InvalidWord do
  defexception word: nil
  def message(e), do: "invalid word: #{inspect(e.word)}"
end

defmodule Forth.UnknownWord do
  defexception word: nil
  def message(e), do: "unknown word: #{inspect(e.word)}"
end

defmodule Forth.DivisionByZero do
  defexception []
  def message(_), do: "division by zero"
end
