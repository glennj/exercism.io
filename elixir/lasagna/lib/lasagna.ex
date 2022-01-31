defmodule Lasagna do
  def expected_minutes_in_oven(), do: 40
  def alarm(), do: "Ding!"

  defp preparation_time_per_layer(), do: 2

  def remaining_minutes_in_oven(mins_in_oven) do
    expected_minutes_in_oven() - mins_in_oven
  end

  def preparation_time_in_minutes(num_layers) do
    num_layers * preparation_time_per_layer()
  end

  def total_time_in_minutes(num_layers, mins_in_oven) do
    preparation_time_in_minutes(num_layers) + mins_in_oven
  end
end
