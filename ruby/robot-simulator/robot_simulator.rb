# class comment
class Robot
  # this particular ordering used by #advance
  BEARINGS = %i[east north west south].freeze

  attr_reader :bearing, :coordinates

  def at(*coords)
    @coordinates = coords
  end

  def orient(bearing)
    raise ArgumentError unless BEARINGS.include? bearing
    @bearing = bearing
  end

  def turn_right
    turn(-1)
  end

  def turn_left
    turn(+1)
  end

  def turn(direction)
    idx = BEARINGS.index bearing
    new_idx = (idx + direction) % BEARINGS.length
    orient BEARINGS[new_idx]
  end

  def advance
    rad = (BEARINGS.index(bearing) * 90) * (2 * Math::PI / 360)
    x, y = coordinates
    dx = Math.cos(rad).to_i
    dy = Math.sin(rad).to_i
    at x + dx, y + dy
  end
end

# class comment
class Simulator
  INSTRUCTIONS = { 'L' => :turn_left, 'R' => :turn_right, 'A' => :advance }.freeze

  def instructions(script)
    instructions = script.chars
    raise ArgumentError unless instructions.all? { |i| INSTRUCTIONS.key? i }

    instructions.map { |i| INSTRUCTIONS[i] }
  end

  def place(robot, x: 0, y: 0, direction: :north)
    robot.at x, y
    robot.orient direction
  end

  def evaluate(robot, script)
    instructions(script).each do |i|
      robot.send(i)
    end
  end
end
