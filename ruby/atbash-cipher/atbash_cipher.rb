module Atbash
  module_function

  def encode(string)
    string
      .downcase
      .gsub(/[^[:alnum:]]/, '')
      .tr('abcdefghijklmnopqrstuvwxyz',
          'zyxwvutsrqponmlkjihgfedcba')
      .scan(/.{1,5}/)
      .join(' ')
  end
end
