class Alphametics
  private

  attr_reader :words
  attr_reader :letters

  public

  def self.solve(puzzle)
    new(puzzle).solution
  end

  def initialize(puzzle)
    @words = puzzle.scan(/[A-Z]+/)
    @letters = puzzle.scan(/[A-Z]/).uniq.sort
  end

  def solution
    (0..9).to_a.permutation(letters.length) do |digits|
      map = letters.zip(digits).to_h
      operands = words.map { |w| digitize(w, map) }
      next if operands.any? { |op| op.start_with?('0') }

      answer = operands.pop.to_i
      sum = operands.map(&:to_i).sum
      return map if sum == answer
    end
    # no solution, return empty map
    {}
  end

  private

  def digitize(word, map)
    word.chars.reduce('') { |a, b| a + map[b].to_s }
  end
end
