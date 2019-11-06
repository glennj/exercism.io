module Pangram
  module_function

  def pangram?(phrase)
    phrase.downcase.gsub(/[^a-z]/, '').chars.uniq.length == 26
  end
end
