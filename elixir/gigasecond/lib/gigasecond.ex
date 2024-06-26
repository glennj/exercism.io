# with inspiration from 
# https://exercism.org/tracks/elixir/exercises/gigasecond/solutions/jlangr

defmodule Gigasecond do
  alias NaiveDateTime, as: NDT

  @gigasecond 1_000_000_000

  @doc """
  Calculate a date one billion seconds after an input date.
  """
  @spec from({{pos_integer, pos_integer, pos_integer}, {pos_integer, pos_integer, pos_integer}}) ::
          {{pos_integer, pos_integer, pos_integer}, {pos_integer, pos_integer, pos_integer}}
  def from({{year, month, day}, {hours, minutes, seconds}}) do
    with future <-
      NDT.new!(year, month, day, hours, minutes, seconds)
      |> NDT.add(@gigasecond, :second)
    do
      {
        future |> NDT.to_date() |> Date.to_erl(),
        future |> NDT.to_time() |> Time.to_erl()
      }
    end
  end
end
