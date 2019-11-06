class Cipher
  ALPHABET = ('a'..'z').to_a.join('').freeze

  attr_reader :key

  def initialize(key = 'aaaaaaaaaa')
    raise ArgumentError if key.empty? || key =~ /[^[:lower:]]/
    @key = key
  end

  def encode(plaintext)
    xxcode(plaintext, +1)
  end

  def decode(ciphertext)
    xxcode(ciphertext, -1)
  end

  private

  def xxcode(text, direction)
    ensure_key_length text
    Array.new(text.length) do |idx|
      text_idx = ALPHABET.index text[idx]
      key_idx  = ALPHABET.index key[idx]
      ALPHABET[(text_idx + direction * key_idx) % ALPHABET.length]
    end.join('')
  end

  def ensure_key_length(text)
    key << key while key.length < text.length
  end
end
