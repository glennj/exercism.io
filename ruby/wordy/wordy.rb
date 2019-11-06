class WordProblem
  OPS = {
    'plus' => '+',
    'minus' => '-',
    'multiplied by' => '*',
    'divided by' => '/'
  }.freeze

  attr_reader :phrase

  def initialize(phrase)
    @phrase = phrase
  end

  def answer
    @answer ||= calculate parse phrase
    raise ArgumentError if @answer.nil?
    @answer
  end

  private

  def parse(phrase)
    # match a number follwed by an operation word followed by a number
    re = Regexp.new "(-?\\d+\\s+)(#{OPS.keys.join('|')})(\\s+-?\\d+)"
    # replace all the operations
    1 while phrase.sub!(re) { $1 + OPS[$2] + $3 }  # rubocop:disable Style/PerlBackrefs
    # remove remaining words and interrogation
    phrase.gsub!(/[[:alpha:]]+|\?$/, '')
    phrase.strip.split
  end

  def calculate(stack)
    return if stack.length < 3

    result = stack.shift.to_i
    until stack.empty?
      op, operand = stack.shift 2
      return unless OPS.value? op

      result = result.send op.to_s, operand.to_i
    end
    result
  end
end
