class FlowerField
  private

  attr_reader :field

  public

  def self.annotate(input)
    new(input).reveal_count
  end

  def initialize(input)
    @field = input
  end

  def reveal_count
    0.upto(field.length - 1) do |r|
      0.upto(field[r].length - 1) do |c|
        next if field[r][c] == '*'
        flowers = count_flowers(r, c)
        field[r][c] = flowers.to_s unless flowers.zero?
      end
    end
    field
  end

  private

  def count_flowers(row, col)
    count = 0
    [-1, 0, 1].each do |dr|
      [-1, 0, 1].each do |dc|
        count += 1 if (0...field.length).include?(row + dr) and
                      (0...field[row].length).include?(col + dc) and
                      field[row + dr][col + dc] == '*'
      end
    end
    count
  end
end
