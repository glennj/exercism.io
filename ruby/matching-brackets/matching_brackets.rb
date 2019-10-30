module Brackets
  PAIRS = { '}' => '{', ']' => '[', ')' => '(' }.freeze

  module_function

  def paired?(text)
    stack = []
    text.chars.each do |c|
      if PAIRS.value? c
        # an open bracket
        stack.push c
      elsif PAIRS.key? c
        # a close bracket
        return false unless stack.last == PAIRS[c]
        stack.pop
      end
    end
    stack.length.zero?
  end
end
