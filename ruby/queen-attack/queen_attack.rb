class Queens
  private

  attr_reader :queens

  public

  def initialize(**positions)
    raise ArgumentError unless positions.values.all? { |p| on_board p }
    @queens = positions
  end

  def attack?
    dx, dy = [0, 1].map { |i| (queens[:white][i] - queens[:black][i]).abs }
    dx.zero? || dy.zero? || dx == dy
  end

  private

  def on_board((x, y))
    size = (0..7)
    size.cover?(x) && size.cover?(y)
  end
end
