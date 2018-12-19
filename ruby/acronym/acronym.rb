class Acronym
  def self.abbreviate(phrase = '')
    phrase.scan(/\b\w/).join('').upcase
  end
end
