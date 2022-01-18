class Phrase
  attr_reader :word_count

  def initialize(phrase)
    @word_count = Hash.new(0)
    pattern = /[[:alnum:]]+(?:'[[:alpha:]]+)?/
    phrase.downcase.scan(pattern).each do |word|
      @word_count[word] += 1
    end
  end
end
