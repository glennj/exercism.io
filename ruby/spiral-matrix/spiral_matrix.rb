class SpiralMatrix
  attr_reader :size

  def initialize(size)
    @size = size
  end

  def matrix
    m = Array.new(@size) {|_| Array.new(@size)}
    x, y = 0, 0
    dx, dy = 0, 1
    
    1.upto(@size * @size) do |i|
      m[x][y] = i

      if  x + dx < 0 || x + dx == @size ||
          y + dy < 0 || y + dy == @size ||
          m[x + dx][y + dy] != nil
      then
        dx, dy = dy, -dx
      end

      x += dx
      y += dy
    end
    return m
  end
end
