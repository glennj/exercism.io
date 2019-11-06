# monkeypatching, eh?
class Integer
  ROMAN_TO_DECIMAL = {
    'M' => 1000, 'CM' => 900, 'D' => 500, 'CD' => 400,
    'C' => 100,  'XC' => 90,  'L' => 50,  'XL' => 40,
    'X' => 10,   'IX' => 9,   'V' => 5,   'IV' => 4,
    'I' => 1
  }.freeze

  def to_roman
    n = self
    ROMAN_TO_DECIMAL.reduce('') do |result, (roman, value)|
      while n >= value
        n -= value
        result << roman
      end
      result
    end
  end
end
