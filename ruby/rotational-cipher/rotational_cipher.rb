module RotationalCipher
  module_function

  LOWER = 'abcdefghijklmnopqrstuvwxyz'.freeze

  def rotate(message, num)
    num %= LOWER.length
    erlow = LOWER[num..-1] + LOWER[0...num]
    message.tr(LOWER + LOWER.upcase, erlow + erlow.upcase)
  end
end
