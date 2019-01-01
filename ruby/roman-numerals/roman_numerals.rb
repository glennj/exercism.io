# monkeypatching, eh?
class Integer
  def to_roman
    n = self
    {
      'M' => 1000, 'CM' => 900, 'D' => 500, 'CD' => 400,
      'C' => 100,  'XC' => 90,  'L' => 50,  'XL' => 40,
      'X' => 10,   'IX' => 9,   'V' => 5,   'IV' => 4,
      'I' => 1
    }.reduce('') do |result, (roman, value)|
      while n >= value
        n -= value
        result << roman
      end
      result
    end
  end
end
