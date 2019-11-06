class Board
  private

  attr_reader :board

  public

  def self.transform(input)
    new(input).reveal_count
  end

  def initialize(input)
    @board = input
    validate_size
    validate_borders
    validate_contents
  end

  def reveal_count
    1.upto(board.length - 2) do |r|
      1.upto(board[r].length - 2) do |c|
        next if board[r][c] == '*'
        bombs = count_bombs(r, c)
        board[r][c] = bombs.to_s unless bombs.zero?
      end
    end
    board
  end

  private

  def count_bombs(row, col)
    count = 0
    [-1, 0, 1].each do |dr|
      [-1, 0, 1].each do |dc|
        count += 1 if board[row + dr][col + dc] == '*'
      end
    end
    count
  end

  def validate_size
    raise ArgumentError if board.length < 3
    raise ArgumentError if board.first.length < 3
    raise ArgumentError unless board.all? { |row| row.length == board.first.length }
  end

  def validate_borders
    raise ArgumentError unless board.first =~ /^\+-+\+$/
    raise ArgumentError unless board.last =~ /^\+-+\+$/
    raise ArgumentError unless board[1..-2].all? { |row| row =~ /^\|.+\|$/ }
  end

  def validate_contents
    raise ArgumentError unless board[1..-2].all? { |row| row[1..-2] =~ /^[ *]+$/ }
  end
end
