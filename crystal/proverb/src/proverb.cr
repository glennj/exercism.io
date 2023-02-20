module Proverb
  def self.recite(input : Array(String)) : Array(String)
    lines = input
      .each_cons(2)
      .map {|(this, that)| "For want of a #{this} the #{that} was lost."}
      .to_a
    lines << "And all for the want of a #{input.first}." unless input.empty?
    lines
  end
end
