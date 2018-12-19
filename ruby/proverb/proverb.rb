class Proverb
  def initialize(*words, qualifier: '')
    raise ArgumentError if words.length < 2

    @text = words.each_cons(2)
                 .each_with_object('') do |(this, that), text|
                   text << "For want of a #{this} the #{that} was lost.\n"
                 end
    @text << "And all for the want of a #{(qualifier + ' ').lstrip}#{words[0]}."
  end

  attr_reader :text
  alias to_s text
end
