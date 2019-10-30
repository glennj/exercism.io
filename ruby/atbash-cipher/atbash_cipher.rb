module Atbash
  module_function

  def encode(string)
    decode(string)
      .scan(/.{1,5}/)
      .join(' ')
  end

  def decode(string)
    string
      .downcase
      .gsub(/[^[:alnum:]]/, '')
      .tr('abcdefghijklmnopqrstuvwxyz',
          'zyxwvutsrqponmlkjihgfedcba')
  end
end
