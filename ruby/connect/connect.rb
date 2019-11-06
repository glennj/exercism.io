# With significant gratitude to shybyte
# https://exercism.io/tracks/typescript/exercises/connect/solutions/2e4550709a0241e09842d418c0361225

module DeepCopy
  refine Object do
    def deepcopy
      Marshal.load(Marshal.dump(self))
    end
  end
end

# the Connect game win tester
class Board
  using DeepCopy

  def initialize(input)
    @board = input.map { |row| row.split(' ') }
  end

  def winner
    return 'O' if winner?('O', @board.deepcopy)
    return 'X' if winner?('X', @board.transpose)
    '' # no winner
  end

  private

  def winner?(player, board)
    stack = my_players_in_top_row(player, board)
    return false if stack.empty?
    return true if board.length == 1
    path? player, board, stack
  end

  def my_players_in_top_row(player, board)
    board[0].each_with_index
            .select { |pair| pair.first == player }
            .map { |pair| [0, pair.last] }
  end

  def path?(player, board, stack)
    last_row = board.length - 1
    until stack.empty?
      r, c = stack.pop
      neighbours(player, board, r, c).each do |(rr, cc)|
        return true if rr == last_row

        board[r][c] = 'seen'
        stack << [rr, cc]
      end
    end
    false
  end

  def neighbours(player, board, row, col)
    height = board.length
    width = board[0].length
    neighbours = []

    [-1, 0, 1].each do |dr|
      rr = row + dr
      next if rr.negative? || rr >= height

      [-1, 0, 1].each do |dc|
        cc = col + dc
        next if cc.negative? || cc >= width
        next if dr == dc # cannot move this way

        neighbours << [rr, cc] if board[rr][cc] == player
      end
    end
    neighbours
  end
end
