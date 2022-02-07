defmodule Robot do
  @enforce_keys [:direction, :position]
  defstruct [:direction, :position]

  def turn_right(robot) do
    case robot.direction do
      :north -> %{robot | direction: :east}
      :east  -> %{robot | direction: :south}
      :south -> %{robot | direction: :west}
      :west  -> %{robot | direction: :north}
    end
  end

  def turn_left(robot) do
    case robot.direction do
      :north -> %{robot | direction: :west}
      :east  -> %{robot | direction: :north}
      :south -> %{robot | direction: :east}
      :west  -> %{robot | direction: :south}
    end
  end

  def advance(robot) do
    {x, y} = robot.position
    case robot.direction do
      :north -> %{robot | position: {x, y + 1}}
      :east  -> %{robot | position: {x + 1, y}}
      :south -> %{robot | position: {x, y - 1}}
      :west  -> %{robot | position: {x - 1, y}}
    end
  end
end
