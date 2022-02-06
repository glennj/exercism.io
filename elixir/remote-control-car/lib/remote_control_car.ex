defmodule RemoteControlCar do
  @enforce_keys [:nickname]

  defstruct [
    :nickname,
    battery_percentage: 100,
    distance_driven_in_meters: 0,
  ]

  def new(nickname \\ "none") do
    %RemoteControlCar{nickname: nickname}
  end

  def display_distance(car) when is_struct(car, RemoteControlCar) do
    "#{car.distance_driven_in_meters} meters"
  end

  def display_battery(car) when is_struct(car, RemoteControlCar) do
    case car.battery_percentage do
      0 -> "Battery empty"
      p -> "Battery at #{p}%"
    end
  end

  def drive(car) when is_struct(car, RemoteControlCar) do
    do_drive(car)
  end

  defp do_drive(car) when car.battery_percentage == 0, do: car
  defp do_drive(car) do
    car
    |> Map.update(:distance_driven_in_meters, 0, &(&1 + 20))
    |> Map.update(:battery_percentage, 0, &(&1 - 1))
  end
end
