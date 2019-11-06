class Matrix
  attr_reader :rows, :columns

  private

  attr_reader :r_max, :c_min

  public

  def initialize(string)
    @rows = string.lines.map { |row| row.split(' ').map(&:to_i) }
    @columns = @rows.transpose
    @r_max = @rows.map(&:max)
    @c_min = @columns.map(&:min)
  end

  def saddle_points
    sp = []
    rows.length.times do |x|
      columns.length.times do |y|
        value = rows[x][y]
        sp << [x, y] if value == r_max[x] && value == c_min[y]
      end
    end
    sp
  end
end
