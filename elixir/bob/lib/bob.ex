defmodule Bob do
  @spec hey(String.t()) :: String.t()
  def hey(input) do
    case {silence?(input), shouting?(input), question?(input)} do
      {true, _, _} -> "Fine. Be that way!"
      {_, true, true} -> "Calm down, I know what I'm doing!"
      {_, true, _} -> "Whoa, chill out!"
      {_, _, true} -> "Sure."
      {_, _, _} -> "Whatever."
    end
  end

  defp silence?(input) do
    String.replace(input, ~r/\s/, "") == ""
  end

  defp question?(input) do
    input
    |> String.trim_trailing()
    |> String.ends_with?("?")
  end

  defp shouting?(input) do
    cond do
      input =~ ~r/[[:lower:]]/ -> false
      input =~ ~r/[[:upper:]]/ -> true
      true -> false
    end
  end
end
