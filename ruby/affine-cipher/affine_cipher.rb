class Integer
  ## find `n` where `a*n mod m == 1`
  def modular_multiplicative_inverse(m)
    raise ArgumentError, "#{self} and #{m} are not coprime" unless gcd(m) == 1

    # this simple approach is brute force, not the most efficient.
    n = 0
    n += 1 until send(:*, n).modulo(m) == 1
    n
  end
end

class Affine
  ALPHABET = ('a'..'z').to_a.join('').freeze

  def initialize(a, b)
    @mmi = a.modular_multiplicative_inverse ALPHABET.length
    @coefficients = [a, b]
  end

  def encode(plaintext)
    encoded = xxcode(plaintext) { |x| E(x) }
    encoded.scan(/.{1,5}/).join(' ')
  end

  def decode(ciphertext)
    xxcode(ciphertext) { |x| D(x) }
  end

  private

  def xxcode(text, &block)
    t = text.downcase.gsub(/[^[:alnum:]]/, '')
    t.chars.map(&block).join('')
  end

  def E(char)
    x = ALPHABET.index char
    return char if x.nil?

    a, b = @coefficients
    m = ALPHABET.length
    ALPHABET[ (a * x + b) % m ]
  end

  def D(char)
    y = ALPHABET.index char
    return char if y.nil?

    _a, b = @coefficients
    m = ALPHABET.length
    a_inv = @mmi
    ALPHABET[ (a_inv * (y - b)) % m ]
  end
end
