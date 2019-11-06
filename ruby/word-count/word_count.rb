class Phrase
  attr_reader :phrase

  def initialize(phrase)
    @phrase = phrase
    @word_count = Hash.new(0)
    pattern = /[[:alnum:]]+(?:'[[:alpha:]]+)?/
    phrase.downcase.scan(pattern).each do |word|
      @word_count[word] += 1
    end
  end

  def word_count
    @word_count.clone
  end
end
