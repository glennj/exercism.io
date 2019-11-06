class Isogram
  def self.isogram?(input)
    scrubbed = input.downcase.gsub(/[^[:alpha:]]/, '')
    chars = scrubbed.chars.sort.join.squeeze
    scrubbed.length == chars.length
  end
end
