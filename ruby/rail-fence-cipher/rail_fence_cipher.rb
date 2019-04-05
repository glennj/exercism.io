# Reimplementation based on:
# https://exercism.io/tracks/python/exercises/rail-fence-cipher/solutions/8d7425bdbb844c5e9416015cd7eb3daa

class RailFenceCipher
  def self.encode(plaintext, rails)
    railed_indices(plaintext, rails)
      .map {|i| plaintext[i]}
      .join('')
  end

  def self.decode(ciphertext, rails)
    railed_indices(ciphertext, rails)
      .zip(ciphertext.chars)
      .sort_by {|(i, char)| i}
      .map {|(i, char)| char}
      .join('')
  end

  private

  # Generate the pattern of rails.
  # Ex: given n = 3, we return:
  #  {1, 2, 3, 2, 1, 2, 3, 2, ...}
  def self.rail_pattern(n)
    r = (1..n).to_a
    r += r[1..-2].reverse
    r.cycle
  end

  # Generate the indices required for encoding.
  #
  # Ex. given text = 'HELLOWORLD' and n = 3, then rails are
  #
  #   0 1 2 3 4 5 6 7 8 9
  # 1 H . . . O . . . L .
  # 2 . E . L . W . R . D
  # 3 . . L . . . O . . .
  #
  # The encoded value is "HOLELWRDLO"
  #
  # This function returns the array:
  #  {0, 4, 8, 1, 3, 5, 7, 9, 2, 6}
  def self.railed_indices(text, n)
    rp = rail_pattern(n)
    return (0...text.length).sort_by {|i| [rp.next, i]}
  end
end



## previous take:
##
## class RailFenceCipher
##   def self.encode(plaintext, num_rails)
##     new(num_rails).encode(plaintext)
##   end
##
##   def self.decode(ciphertext, num_rails)
##     new(num_rails).decode(ciphertext)
##   end
##
##   def initialize(num_rails)
##     @nr = num_rails
##   end
##
##   def encode(plaintext)
##     return plaintext if @nr == 1 || @nr > plaintext.length
##
##     rails = Array.new(@nr) { '' }
##     rail_idx = init_incr_rail
##     plaintext.length.times do |i|
##       rails[rail_idx] << plaintext[i]
##       rail_idx = incr_rail rail_idx
##     end
##     rails.join('')
##   end
##
##   def decode(ciphertext)
##     return ciphertext if @nr == 1 || @nr > ciphertext.length
##
##     rails = partition_ciphertext(ciphertext)
##     plaintext = ''
##     rail_idx = init_incr_rail
##     ciphertext.length.times do
##       plaintext << rails[rail_idx].slice!(0, 1)
##       rail_idx = incr_rail rail_idx
##     end
##     plaintext
##   end
##
##   private
##
##   def partition_ciphertext(ciphertext)
##     rail_lengths = partition_lengths(ciphertext)
##     copy = ciphertext.dup
##     rail_lengths.map { |len| copy.slice!(0, len) }
##   end
##
##   def partition_lengths(ciphertext)
##     # i. each journey down and up the rails consumes 2 * (num_rails - 1) characters
##     # ii. the "inner" rails consume twice as many as the top & bottom rails
##     base_rail_len, leftover = ciphertext.length.divmod(2 * (@nr - 1))
##     lengths = Array.new(@nr) { 2 * base_rail_len }
##     lengths[0] = lengths[@nr - 1] = base_rail_len
##     # handle the leftovers, 1 char per rail starting from the top
##     rail_idx = init_incr_rail
##     leftover.times do
##       lengths[rail_idx] += 1
##       rail_idx = incr_rail rail_idx
##     end
##     lengths
##   end
##
##   def init_incr_rail
##     @inc = 1
##     0
##   end
##
##   def incr_rail(rail)
##     rail += @inc
##     return rail unless [-1, @nr].include? rail
##     # gone too far, change direction
##     @inc *= -1
##     rail + 2 * @inc
##   end
## end
