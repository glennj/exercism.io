class RobotSimulator
  @position : Position
  @bearing : Bearing

  def initialize(coordinates, direction)
    @position = Position.new *coordinates
    @bearing = Bearing.new direction
  end

  def x
    @position.x
  end

  def y
    @position.y
  end

  def direction
    @bearing.direction
  end

  def move(instructions : String)
    instructions.each_char do |instruction|
      case instruction
      when 'R' then @bearing.turn_right
      when 'L' then @bearing.turn_left
      when 'A' then @position.advance(@bearing)
      end
    end
  end
end
