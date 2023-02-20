module AtbashCipher
  def self.encode(message : String) : String
    decode(message)
      .scan(/.{1,5}/)
      .map(&.[0])
      .join(" ")
  end

  def self.decode(message : String) : String
    message
      .downcase
      .gsub(/[^[:alnum:]]/, "")
      .tr("abcdefghijklmnopqrstuvwxyz",
          "zyxwvutsrqponmlkjihgfedcba")
  end
end
