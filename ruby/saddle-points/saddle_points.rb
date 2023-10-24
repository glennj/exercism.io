class Grid
  def self.saddle_points(input)
    self.new(input).saddle_points
  end

  private

  attr_reader :rows, :columns, :r_max, :c_min

  public

  def initialize(input)
    @rows = input
    @columns = @rows.transpose
    @r_max = @rows.map(&:max)
    @c_min = @columns.map(&:min)
  end

  def saddle_points
    sp = []
    rows.length.times do |x|
      columns.length.times do |y|
        value = rows[x][y]
        if value == r_max[x] && value == c_min[y]
          sp << {"row" => x + 1, "column" => y + 1}
        end
      end
    end
    sp
  end
end
