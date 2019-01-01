class Isogram
  def self.isogram?(input)
    scrubbed = input.downcase.gsub(/[^[:alpha:]]/, '')
    chars = scrubbed.chars.sort.join.squeeze
    scrubbed.length == chars.length    
  end
end

=begin
  nice
    c = input.downcase.gsub(/[^[:alpha:]]/, '').chars
    c.length == c.uniq.length
=end