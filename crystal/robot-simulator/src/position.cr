class Position
  getter x : Int32
  getter y : Int32

  def initialize(@x, @y)
  end

  def advance(bearing)
    rad = bearing.to_radians
    @x += Math.cos(rad).to_i
    @y += Math.sin(rad).to_i
  end
end
