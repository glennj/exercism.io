# Don't monkeypatch. Use a refinement
module IntegerPrimality
  refine Integer do
    def prime?
      return false if self < 2
      return true  if self == 2
      return false if even?

      sqrt = Math.sqrt(self).floor
      (3..sqrt).step(2).each do |i|
        return false if (self % i).zero?
      end
      true
    end
  end
end

# comment
module Prime
  using IntegerPrimality

  @@primes = [nil, 2, 3]

  module_function

  def next_prime(p)
    loop do
      p += 2
      break if p.prime?
    end
    p
  end

  def nth(n)
    raise ArgumentError if n < 1
    @@primes << next_prime(@@primes.last) while @@primes.length <= n
    @@primes[n]
  end
end
