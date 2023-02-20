class Hash
  def reverse
    self.map{|key, value| {value, key}}.to_h
  end
end

class Bearing
  BEARINGS = {:north => 90, :south => 270, :east => 0, :west => 180}

  @bearing : Int32

  def initialize(direction)
    raise ArgumentError.new unless BEARINGS.has_key? direction
    @bearing = BEARINGS[direction]
  end

  def direction
    BEARINGS.reverse.[@bearing]
  end

  def turn_right
    turn -1
  end

  def turn_left
    turn +1
  end

  private def turn(dir)
    @bearing += 90 * dir
    @bearing %= 360
  end

  def to_radians
    Math::TAU * @bearing / 360
  end
end
