defmodule Bowling do
  defstruct frame_no: 1, frame: [], bonuses: [], score: 0

  @spec start() :: any
  def start do
    %__MODULE__{}
  end

  @spec score(any) :: {:ok, integer} | {:error, String.t()}
  def score(game) when game.frame_no > 10, do: {:ok, game.score}
  def score(_game), do: {:error, "Score cannot be taken until the end of the game"}

  @spec roll(any, integer) :: {:ok, any} | {:error, String.t()}
  def roll(_game, roll) when roll > 10, do: {:error, "Pin count exceeds pins on the lane"}
  def roll(_game, roll) when roll < 0, do: {:error, "Negative roll is invalid"}
  def roll(game, _roll) when game.frame_no > 10, do: {:error, "Cannot roll after game is over"}

  def roll(game, roll) do
    game = update_score(game, roll)

    case {roll, game.frame, game.frame_no} do
      {_, _, 10} ->
        tenth_frame_roll(game, roll)

      {10, [], _} ->
        # strike
        {:ok, game |> new_bonus(2) |> next_frame()}

      {_, [], _} ->
        {:ok, game |> register_roll(roll)}

      {_, [roll1], _} when roll1 + roll == 10 ->
        # spare
        {:ok, game |> new_bonus(1) |> next_frame()}

      {_, [roll1], _} when roll1 + roll > 10 ->
        {:error, "Pin count exceeds pins on the lane"}

      {_, _, _} ->
        {:ok, game |> next_frame()}
    end
  end

  defp tenth_frame_roll(game, roll) do
    case game.frame do
      [roll1] when roll1 + roll < 10 ->
        {:ok, game |> next_frame()}

      [roll1] when roll1 != 10 and roll1 + roll > 10 ->
        {:error, "Pin count exceeds pins on the lane"}

      [10, 10] ->
        {:ok, game |> next_frame()}

      [roll2, 10] when roll2 + roll > 10 ->
        {:error, "Pin count exceeds pins on the lane"}

      [_, 10] ->
        {:ok, game |> next_frame()}

      [roll2, roll1] when roll1 + roll2 == 10 ->
        {:ok, game |> next_frame()}

      _ ->
        {:ok, game |> register_roll(roll)}
    end
  end

  # ------------------------------------------------------------

  defp update_score(game, roll) do
    game
    |> Map.update!(:score, &(&1 + roll))
    |> add_bonus_rolls(roll)
  end

  defp add_bonus_rolls(game, roll) do
    bonuses = Enum.filter(game.bonuses, &(&1 > 0))

    game
    |> Map.update!(:bonuses, fn bs -> Enum.map(bs, &max(&1 - 1, 0)) end)
    |> Map.update!(:score, &(&1 + roll * length(bonuses)))
  end

  defp register_roll(game, roll) do
    Map.update!(game, :frame, &[roll | &1])
  end

  defp next_frame(game) do
    game
    |> Map.update!(:frame_no, &(&1 + 1))
    |> Map.put(:frame, [])
  end

  defp new_bonus(game, size) do
    Map.update!(game, :bonuses, &[size | &1])
  end
end
