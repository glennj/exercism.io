defmodule RotationalCipher do
  @doc """
  Given a plaintext and amount to shift by, return a rotated string.
  """
  @spec rotate(text :: String.t(), shift :: integer) :: String.t()
  def rotate(text, shift) do
    map = get_map(shift)

    text
    |> String.split("")
    |> Enum.map(&rotated(&1, map))
    |> Enum.join("")
  end

  defp get_map(shift) do
    alphabet = ~w/a b c d e f g h i j k l m n o p q r s t u v w x y z/
    shf = rem(shift + 26, 26)
    rotated = Enum.drop(alphabet, shf) ++ Enum.take(alphabet, shf)

    Enum.zip(
      alphabet ++ Enum.map(alphabet, &String.upcase/1),
      rotated ++ Enum.map(rotated, &String.upcase/1)
    )
    |> Enum.into(%{})
  end

  defp rotated(char, map) do
    case map[char] do
      nil -> char
      x -> x
    end
  end
end
