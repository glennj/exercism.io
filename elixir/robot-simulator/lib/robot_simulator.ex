defmodule RobotSimulator do
  @type direction :: :north | :east | :south | :west
  @type position :: {integer, integer}

  @doc """
  Create a Robot Simulator given an initial direction and position.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec create(direction :: direction, position :: position) :: Robot
  def create(direction \\ :north, position \\ {0, 0}) do
    cond do
      not valid_direction(direction) -> {:error, "invalid direction"}
      not valid_position(position) -> {:error, "invalid position"}
      true -> %Robot{direction: direction, position: position}
    end
  end

  defp valid_direction(dir), do: dir in [:north, :east, :south, :west]

  defp valid_position(pos) do
      is_tuple(pos)
      and tuple_size(pos) == 2
      and is_integer(elem(pos, 0))
      and is_integer(elem(pos, 1))
  end

  @doc """
  Return the robot's direction.
  """
  @spec direction(robot :: Robot) :: direction
  def direction(robot), do: robot.direction

  @doc """
  Return the robot's position.
  """
  @spec position(robot :: Robot) :: position
  def position(robot), do: robot.position

  @doc """
  Simulate the robot's movement given a string of instructions.

  Valid instructions are: "R" (turn right), "L", (turn left), and "A" (advance)
  """
  @spec simulate(robot :: Robot, instructions :: String.t()) :: Robot
  def simulate(robot, instructions) do
    do_simulate(robot, to_charlist(instructions))
  end

  defp do_simulate(robot, ''), do: robot

  defp do_simulate(robot, [instr | instructions]) do
    case instr do
      ?R -> do_simulate(Robot.turn_right(robot), instructions)
      ?L -> do_simulate(Robot.turn_left(robot), instructions)
      ?A -> do_simulate(Robot.advance(robot), instructions)
       _ -> {:error, "invalid instruction"}
    end
  end
end
