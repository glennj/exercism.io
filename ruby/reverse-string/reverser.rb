module Reverser
  def reverse(str)
    str.chars.reduce([]) {|reversed, ch| reversed.unshift(ch)}.join
  end
  module_function :reverse
end
