# combination of this lovely recursive solution
# https://exercism.io/tracks/javascript/exercises/say/solutions/515ab00bc90f46b0bde3732d9317a46b
# and this wondrous ruby solution
# https://exercism.io/tracks/ruby/exercises/say/solutions/b560c2532a2042dea97a414ffe7fc9b5

# module comment
module InEnglish
  SMALL = %w[
    zero one two three four five six seven eight nine ten eleven
    twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen
  ].freeze

  XTY = {
    20 => 'twenty', 30 => 'thirty',  40 => 'forty',  50 => 'fifty',
    60 => 'sixty',  70 => 'seventy', 80 => 'eighty', 90 => 'ninety'
  }.freeze

  refine Integer do
    def say
      raise ArgumentError unless (0...1e12.to_i).cover? self

      if    self < 100 then say_small
      elsif self < 1e3 then say_compound 100, 'hundred'
      elsif self < 1e6 then say_compound 1e3, 'thousand'
      elsif self < 1e9 then say_compound 1e6, 'million'
      else                  say_compound 1e9, 'billion'
      end
    end

    private

    def say_small
      SMALL[self] || XTY[self] || begin
        units = modulo(10)
        "#{(self - units).say}-#{units.say}"
      end
    end

    def say_compound(base, word)
      num, rem = (divmod base).map(&:to_i) # scientific notation means float division
      [num.say, word, rem.nonzero?&.say].compact.join(' ')
    end
  end
end
