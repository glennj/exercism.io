# translation of this lovely recursive solution
# https://exercism.io/tracks/javascript/exercises/say/solutions/515ab00bc90f46b0bde3732d9317a46b

# more ruby-ish here: 
# https://exercism.io/tracks/ruby/exercises/say/solutions/b560c2532a2042dea97a414ffe7fc9b5

class Say
  SMALL = %w[
    zero one two three four five six seven eight nine ten eleven
    twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen
  ].freeze

  XTY = {
    20 => 'twenty', 30 => 'thirty',  40 => 'forty',  50 => 'fifty',
    60 => 'sixty',  70 => 'seventy', 80 => 'eighty', 90 => 'ninety'
  }.freeze

  def initialize(number)
    @n = number
  end

  def in_english
    say @n
  end

  private

  def say(n)
    raise ArgumentError unless (0..999_999_999_999).cover? n

    if    n < 100  then say_small n
    elsif n < 1e3  then say_compound n, 100, 'hundred'
    elsif n < 1e6  then say_compound n, 1e3, 'thousand'
    elsif n < 1e9  then say_compound n, 1e6, 'million'
    else                say_compound n, 1e9, 'billion'
    end
  end

  def say_small(n)
    SMALL[n] || XTY[n] || "#{say(n - n % 10)}-#{say(n % 10)}"
  end

  def say_compound(n, base, word)
    n, rem = (n.divmod base).map(&:to_i) # scientific notation means float division
    [
      say(n),
      word,
      rem.zero? ? nil : say(rem)
    ].compact.join(' ')
  end
end
