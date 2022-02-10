defmodule Isogram do
  @doc """
  Determines if a word or sentence is an isogram
  """
  @spec isogram?(String.t()) :: boolean
  def isogram?(sentence) do
    chars =
      sentence
      |> String.replace(~r/[\s-]/, "")
      |> String.downcase()
      |> String.graphemes()

    do_isogram?(chars, MapSet.new())
  end

  defp do_isogram?([], _), do: true

  defp do_isogram?([char | chars], seen) do
    cond do
      MapSet.member?(seen, char) -> false
      true -> do_isogram?(chars, MapSet.put(seen, char))
    end
  end
end
