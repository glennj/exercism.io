# Sieve of Eratosthenes to find prime numbers
class Sieve
  attr_reader :primes

  def initialize(limit)
    @limit = limit
    @primes = []
    eratosthenes if limit > 1
  end

  private

  def eratosthenes
    @primes = (2..@limit).to_a
    remove_multiples(2)
    Range.new(3, Math.sqrt(@limit).floor).step(2) do |i|
      remove_multiples(i) unless Math.sqrt(i).integer?
    end
  end

  def remove_multiples(num)
    step = num * (num == 2 ? 1 : 2)
    Range.new(num**2, @limit).step(step) { |i| @primes.delete(i) }
  end
end
