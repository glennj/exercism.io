class RailFenceCipher
  def self.encode(plaintext, num_rails)
    new(num_rails).encode(plaintext)
  end

  def self.decode(ciphertext, num_rails)
    new(num_rails).decode(ciphertext)
  end

  def initialize(num_rails)
    @nr = num_rails
  end

  def encode(plaintext)
    return plaintext if @nr == 1 || @nr > plaintext.length

    rails = Array.new(@nr) { '' }
    rail_idx = init_incr_rail
    plaintext.length.times do |i|
      rails[rail_idx] << plaintext[i]
      rail_idx = incr_rail rail_idx
    end
    rails.join('')
  end

  def decode(ciphertext)
    return ciphertext if @nr == 1 || @nr > ciphertext.length

    rails = partition_ciphertext(ciphertext)
    plaintext = ''
    rail_idx = init_incr_rail
    ciphertext.length.times do
      plaintext << rails[rail_idx].slice!(0, 1)
      rail_idx = incr_rail rail_idx
    end
    plaintext
  end

  private

  def partition_ciphertext(ciphertext)
    rail_lengths = partition_lengths(ciphertext)
    copy = ciphertext.dup
    rail_lengths.map { |len| copy.slice!(0, len) }
  end

  def partition_lengths(ciphertext)
    # i. each journey down and up the rails consumes 2 * (num_rails - 1) characters
    # ii. the "inner" rails consume twice as many as the top & bottom rails
    base_rail_len, leftover = ciphertext.length.divmod(2 * (@nr - 1))
    lengths = Array.new(@nr) { 2 * base_rail_len }
    lengths[0] = lengths[@nr - 1] = base_rail_len
    # handle the leftovers, 1 char per rail starting from the top
    rail_idx = init_incr_rail
    leftover.times do
      lengths[rail_idx] += 1
      rail_idx = incr_rail rail_idx
    end
    lengths
  end

  def init_incr_rail
    @inc = 1
    0
  end

  def incr_rail(rail)
    rail += @inc
    return rail unless [-1, @nr].include? rail
    # gone too far, change direction
    @inc *= -1
    rail + 2 * @inc
  end
end
