defmodule Yacht do
  @type category ::
          :ones
          | :twos
          | :threes
          | :fours
          | :fives
          | :sixes
          | :full_house
          | :four_of_a_kind
          | :little_straight
          | :big_straight
          | :choice
          | :yacht

  @doc """
  Calculate the score of 5 dice using the given category's scoring method.
  """
  @spec score(category :: category(), dice :: [integer]) :: integer
  def score(category, dice) do
    case category do
      :yacht -> group_dice(dice) |> score_yacht()
      :ones -> group_dice(dice) |> score_face(1)
      :twos -> group_dice(dice) |> score_face(2)
      :threes -> group_dice(dice) |> score_face(3)
      :fours -> group_dice(dice) |> score_face(4)
      :fives -> group_dice(dice) |> score_face(5)
      :sixes -> group_dice(dice) |> score_face(6)
      :full_house -> group_dice(dice) |> score_full_house(Enum.sum(dice))
      :four_of_a_kind -> group_dice(dice) |> score_four_of_a_kind()
      :little_straight -> Enum.sort(dice) |> score_straight([1, 2, 3, 4, 5])
      :big_straight -> Enum.sort(dice) |> score_straight([2, 3, 4, 5, 6])
      :choice -> Enum.sum(dice)
    end
  end

  defp group_dice(dice) do
    for die <- dice, reduce: %{} do
      groups -> Map.update(groups, die, 1, &(&1 + 1))
    end
  end

  defp score_yacht(groups) when map_size(groups) == 1, do: 50
  defp score_yacht(_), do: 0

  defp score_face(groups, face) do
    Map.get(groups, face, 0) * face
  end

  defp score_full_house(groups, sum) do
    case Map.values(groups) do
      [2, 3] -> sum
      [3, 2] -> sum
      _ -> 0
    end
  end

  defp score_four_of_a_kind(groups) do
    cond do
      map_size(groups) == 1 ->
        4 * hd(Map.keys(groups))

      Map.values(groups) in [[1, 4], [4, 1]] ->
        [d1, d2] = Map.keys(groups)
        4 * if(groups[d1] == 4, do: d1, else: d2)

      true ->
        0
    end
  end

  defp score_straight(expected, expected), do: 30
  defp score_straight(_, _), do: 0
end
