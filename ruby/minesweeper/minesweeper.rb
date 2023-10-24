class Minesweeper
  private

  attr_reader :board

  public

  def self.annotate(input)
    new(input).reveal_count
  end

  def initialize(input)
    @board = input
  end

  def reveal_count
    0.upto(board.length - 1) do |r|
      0.upto(board[r].length - 1) do |c|
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
        count += 1 if (0...board.length).include?(row + dr) and
                      (0...board[row].length).include?(col + dc) and
                      board[row + dr][col + dc] == '*'
      end
    end
    count
  end
end
