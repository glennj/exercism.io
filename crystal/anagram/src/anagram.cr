module Anagram
  def self.find(subject : String, candidates : Array(String)) : Array(String)
    subj_lc = subject.downcase
    subj_key = key(subject)
    candidates.select {|word| word.downcase != subj_lc && key(word) == subj_key}
  end

  private def self.key(word)
    word.downcase.chars.sort
  end
end
