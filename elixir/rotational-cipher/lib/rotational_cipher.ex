defmodule RotationalCipher do
  @lower Enum.to_list(?a..?z)
  @upper Enum.to_list(?A..?Z)

  @doc """
  Given a plaintext and amount to shift by, return a rotated string.

      iex> RotationalCipher.rotate("Hello", 1)
      "Ifmmp"
      iex> RotationalCipher.rotate("Hello", 25)
      "Gdkkn"
  """
  @spec rotate(text :: String.t(), shift :: integer) :: String.t()
  def rotate(text, shift) do
    map = cipher(shift)

    text
    |> to_charlist()
    |> Enum.map(&Map.get(map, &1, &1))
    |> to_string()
  end

  defp cipher(shift) do
    s = Integer.mod(shift, length(@lower))

    rot_lower = Enum.drop(@lower, s) ++ Enum.take(@lower, s)
    rot_upper = Enum.drop(@upper, s) ++ Enum.take(@upper, s)

    Enum.zip(@lower ++ @upper, rot_lower ++ rot_upper)
    |> Enum.into(%{})
  end

  @doc """
  The famous rot13 cipher

      iex> RotationalCipher.rot13("Hello")
      "Uryyb"
  """
  def rot13(text), do: rotate(text, 13)
end
