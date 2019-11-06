class Crypto
  private

  attr_reader :plaintext

  public

  def initialize(string)
    @plaintext = string.downcase.gsub(/[^[:alnum:]]/, '')
  end

  def ciphertext
    plaintext.length <= 1 ? plaintext : encode
  end

  private

  def encode
    n = Math.sqrt(plaintext.length).ceil
    rows = plaintext.scan(/.{1,#{n}}/)
    # pad the last row with spaces
    rows << (rows.pop + (' ' * n)).slice(0, n)
    # transpose the square
    rows.map(&:chars)
        .transpose
        .map(&:join)
        .join(' ')
  end
end
