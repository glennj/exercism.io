class Anagram
  private

  attr_reader :string
  attr_reader :key

  public

  def initialize(string)
    @string = string.downcase
    @key = to_key(string)
  end

  def match(words)
    words.select { |w| w.downcase != string && to_key(w) == key }
  end

  private

  def to_key(string)
    string.downcase.chars.sort
  end
end
